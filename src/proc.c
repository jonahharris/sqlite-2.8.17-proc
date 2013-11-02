/*
** 2008 June 8
**
** The author disclaims copyright to this source code.  In place of
** a legal notice, here is a blessing:
**
**    May you do good and not evil.
**    May you find forgiveness for yourself and forgive others.
**    May you share freely, never taking more than you give.
**
*************************************************************************
** This file contains routines used for analyzing procedures and
** for generating VDBE code that evaluates procedures in SQLite.
**
** 
*/
#include "sqliteInt.h"
#include <ctype.h>

/*************************************************************************
** 
** Routines to assist in building the parse tree for procedural blocks
*/

void sqliteStartBlock(
  Parse *pParse,    /* Parser context */
  int params		/* true if we are starting with parsing parameters */
){
  Block *pBlock;

  pBlock = sqliteMalloc( sizeof(Block) );
  if( pBlock==0 ) return;

  pBlock->params = params;
  pBlock->pParent = pParse->pCurrentBlock;
  if( pBlock->pParent ) {
  	pBlock->mReturn = pBlock->pParent->mReturn;
  }
  else {
	  pBlock->mReturn = pParse->nMem++;
  }
  pParse->pCurrentBlock = pBlock;
}

Block *sqliteEndBlock(
  Parse *pParse,     /* Parser context */
  StmtList *pStList, /* Statements for this block */
  StmtList *pExList  /* Exception handlers */
){
  Block *pBlock;
  int i;

  pBlock = pParse->pCurrentBlock;
  if( pBlock==0 ) return 0;

  for(i=0; i<pBlock->nVar; i++) {
	  if( pBlock->aVar[i].notNull && pBlock->aVar[i].pDflt==0 ){
      sqliteErrorMsg(pParse, "no default for variable: %s", pBlock->aVar[i].zName);
	  }
  }

  pBlock->pStList = pStList;
  pBlock->pExList = pExList;
  pParse->pCurrentBlock = pBlock->pParent;

  return pBlock;
}

void sqliteBlockDelete(Block *pBlock){
  int i;

  if( pBlock==0 ) return;
  for( i=0; i<pBlock->nVar; i++ ) {
   sqliteFree( pBlock->aVar[i].zName );
  }
  sqliteFree( pBlock->aVar );
  sqliteStmtListDelete(pBlock->pStList );
  sqliteStmtListDelete(pBlock->pExList );
  sqliteFree( pBlock );
}

/*
** Add a new local variable to the block currently being constructed.
**
** The parser calls this routine once for each variable declaration
** in a DECLARE ... BEGIN statement.  sqliteStartBlock() gets called
** first to get things going.  Then this routine is called for each
** variable.
*/
void sqliteAddProcVar(Parse *pParse, Token *pName){
  Block *p;
  int i;
  char *z = 0;
  Variable *pVar;

  if( (p = pParse->pCurrentBlock)==0 ) return;
  sqliteSetNString(&z, pName->z, pName->n, 0);
  if( z==0 ) return;
  sqliteDequote(z);
  for(i=0; i<p->nVar; i++){
    if( strcmp(z, p->aVar[i].zName)==0 ){
      sqliteErrorMsg(pParse, "duplicate variable name: %s", z);
      sqliteFree(z);
      return;
    }
  }
  if( (p->nVar & 0x7)==0 ){
    Variable *aNew;
    aNew = sqliteRealloc( p->aVar, (p->nVar+8)*sizeof(p->aVar[0]));
    if( aNew==0 ) return;
    p->aVar = aNew;
  }
  pVar = &p->aVar[p->nVar];
  memset(pVar, 0, sizeof(p->aVar[0]));
  pVar->zName = z;
  pVar->mVar = pParse->nMem++;
  pVar->isParam = p->params;
  p->nVar++;
}

/*
** This routine is called by the parser while in the middle of
** parsing a variable declaration in a procedure block.  The pFirst
** token is the first token in the sequence of tokens that describe
** the type of the variable currently under construction.   pLast is
** the last token in the sequence.  Use this information to
** construct a string that contains the typename of the variable
** and store that string in zType.
*/ 
void sqliteAddProcVarType(Parse *pParse, Token *pFirst, Token *pLast){
  Block *p;
  int i, j;
  int n;
  char *z, **pz;
  Variable *pVar;

  if( (p = pParse->pCurrentBlock)==0 ) return;
  i = p->nVar-1;
  if( i<0 ) return;
  pVar = &p->aVar[i];
  pz = &pVar->zType;
  n = pLast->n + Addr(pLast->z) - Addr(pFirst->z);
  sqliteSetNString(pz, pFirst->z, n, 0);
  z = *pz;
  if( z==0 ) return;
  for(i=j=0; z[i]; i++){
    int c = z[i];
    if( isspace(c) ) continue;
    z[j++] = c;
  }
  z[j] = 0;
}

/*
** This routine is called by the parser while in the middle of
** parsing a variable declaration in a procedure block.  pExpr is
** the default value for the current variable (if any) and notnull
** is the flag signalling that his variable has NOT NULL as part of
** its type.
*/ 
void sqliteAddProcVarExpr(Parse *pParse, Expr *pExpr, int notnull){
  Block *b;
  int i;

  if( (b = pParse->pCurrentBlock)==0 ) return;
  i = b->nVar-1;
  if( i<0 ) return;
  if( pExpr ) {
    b->aVar[i].pDflt = pExpr;
  }
  if( notnull ) {
	b->aVar[i].notNull = 1;
  }
}

/*
** Construct a new statement node and return a pointer to it.  Memory
** for this node is obtained from sqliteMalloc().  The calling function
** is responsible for making sure the node eventually gets freed.
*/
Stmt* sqliteStmt(
  Parse *pParse,
  int op,
  Expr *pExpr,
  StmtList *pList1,
  Stmt *pStmt2,
  Block *pBlock
){
  Stmt *pNew;
  pNew = sqliteMalloc( sizeof(Stmt) );
  if( pNew==0 ){
    /* When malloc fails, we leak memory */
    return 0;
  }
  pNew->op = op;
  pNew->pExpr1 = pExpr;
  pNew->pStmt1 = pList1;
  pNew->pStmt2 = pStmt2;
  pNew->pBlock = pBlock;

  return pNew;
}

/*
** The parser calls this routine when it sees a SQL statement inside the
** body of a block
*/
Stmt *sqliteSQLStmt(
  int op,			        /* One of TK_SELECT, TK_INSERT, TK_UPDATE, TK_DELETE */
  Token *pTableName,  /* Name of the table into which we insert */
  IdList *pColumn,    /* List of columns in pTableName to insert into */
  ExprList *pEList,   /* The VALUE clause: a list of values to be inserted */
  Select *pSelect,    /* A SELECT statement that supplies values */
  Expr *pWhere,       /* The WHERE clause */
  int orconf          /* The conflict algorithm (OE_Abort, OE_Replace, etc.) */
){
  Stmt *pNew;

  pNew = sqliteMalloc( sizeof(Stmt)+sizeof(SQLStmt) );
  if( pNew==0 ){
    /* When malloc fails, we leak memory */
    return 0;
  }
  pNew->pSql = (SQLStmt*) (pNew+1);

  pNew->op = TK_SQL;
  pNew->pSql->op		  = op;
  pNew->pSql->pSelect   = pSelect;
  if( pTableName ) {
    pNew->pSql->target    = *pTableName;
  }
  pNew->pSql->pIdList   = pColumn;
  pNew->pSql->pExprList = pEList;
  pNew->pSql->pWhere    = pWhere;
  pNew->pSql->orconf	  = orconf;

  return pNew;
}

/*
** Recursively delete a statement tree.
*/
void sqliteStmtDelete(Stmt *p){
  if( p==0 ) return;
  if( p->pExpr1 ) sqliteExprDelete(p->pExpr1);
  if( p->pStmt1 ) sqliteStmtListDelete(p->pStmt1);
  if( p->pStmt2 ) sqliteStmtDelete(p->pStmt2);
  if( p->pBlock ) sqliteBlockDelete(p->pBlock);
}

/*
** Add a new element to the end of a statement list.  If pList is
** initially NULL, then create a new statement list.
*/
StmtList *sqliteStmtListAppend(StmtList *pList, Stmt *pStmt){
  if( pList==0 ){
    pList = sqliteMalloc( sizeof(StmtList) );
    if( pList==0 ){
      /* sqliteStmtDelete(pExpr); // Leak memory if malloc fails */
      return 0;
    }
    assert( pList->nAlloc==0 );
  }
  if( pList->nAlloc<=pList->nStmt ){
    pList->nAlloc = pList->nAlloc*2 + 4;
    pList->a = sqliteRealloc(pList->a, pList->nAlloc*sizeof(pList->a[0]));
    if( pList->a==0 ){
      /* sqliteStmtDelete(pExpr); // Leak memory if malloc fails */
      pList->nStmt = pList->nAlloc = 0;
      return pList;
    }
  }
  assert( pList->a!=0 );
  if( pStmt ){
    struct StmtList_item *pItem = &pList->a[pList->nStmt++];
    memset(pItem, 0, sizeof(*pItem));
    pItem->pStmt = pStmt;
  }
  return pList;
}

/*
** Delete an entire expression list.
*/
void sqliteStmtListDelete(StmtList *pList){
  int i;
  if( pList==0 ) return;
  assert( pList->a!=0 || (pList->nStmt==0 && pList->nAlloc==0) );
  assert( pList->nStmt<=pList->nAlloc );
  for(i=0; i<pList->nStmt; i++) {
    sqliteStmtDelete(pList->a[i].pStmt);
  }
  sqliteFree(pList->a);
  sqliteFree(pList);
}

/*************************************************************************
** 
** Routines to assist in compiling the parse tree into vdbe code
*/

/*
** Given the name of a variable, look up that name in the declarations of the
** current and enclosing blocks and make the pExpr expression node refer back
** to that variable's memory cell.  The following changes are made to pExpr:
**
**    pExpr->iColumn       Set to the number of the memory cell
**    pExpr->op            Set to TK_VAR.
**
** If the name cannot be resolved, leave an error message in pParse and return
** non-zero.  Return zero on success.
*/
int sqliteLookupVar(
  Parse *pParse,      /* The parsing context */
  Block *pBlock,	    /* The current block */
  Expr *pExpr         /* Make this EXPR node point to the selected variable */
){
  Block *b = pBlock;
  char *zVar;

  assert( pExpr->op==TK_ID || pExpr->op==TK_STRING );
  assert( pExpr->pLeft==0 && pExpr->pRight==0 );

  zVar = sqliteStrNDup(pExpr->token.z, pExpr->token.n);
  sqliteDequote(zVar);
  while( b ) {
	  int i;
	  for( i=0; i<b->nVar; i++ ) {
	    if( !strcmp(b->aVar[i].zName, zVar) ) {
          pExpr->iColumn  = b->aVar[i].mVar;
		  pExpr->op = TK_VAR;
		  if( b->aVar[i].notNull ){
		    pExpr->flags = EP_NotNull;
		  }
	      sqliteFree(zVar);
		  return 0;
	    }
	  }
    b = b->pParent;    
  }
  sqliteErrorMsg(pParse, "Variable %s not declared", zVar);
  sqliteFree(zVar);
  return 1;
}

/*
** Recursively walk an expression tree and resolve names to memory
** cell numbers. For assignment expressions, the left espression is
** checked to be an assignable expression (currently only variable
** names are assignable)
*/
int sqliteExprProcResolve(Parse *pParse, Block *pBlock, Expr *pExpr){
  if( pExpr==0 || pBlock==0 ) return 0;
  switch( pExpr->op ){
    /* Double-quoted strings (ex: "abc") are used as identifiers if
    ** possible.  Otherwise they remain as strings.  Single-quoted
    ** strings (ex: 'abc') are always string literals.
    */
    case TK_STRING: {
      if( pExpr->token.z[0]=='\'' ) break;
      /* Fall thru into the TK_ID case if this is a double-quoted string */
    }
    /* A lone identifier is the name of a variable.
    */
    case TK_ID: {
      if( sqliteLookupVar(pParse, pBlock, pExpr) ){
        return 1;
      }
      break; 
    }
  
    /* A dotted name ("X.Y.Z") is not yet allowed in procedural code
    */
    case TK_DOT: {
      sqliteErrorMsg(pParse, "Dotted variable name not allowed yet", 0);
      return 1;
    }

    /* An assignment expression must have a ID node on its left
    */
    case TK_ASSIGN: {
      if( pExpr->pLeft->op!=TK_ID ){
        sqliteErrorMsg(pParse, "Bad lvalue in assignment", 0);
        return 1;
      }
      /* fall through */
    }

	/* For all else, just recursively walk the tree */
    default: {
      if( pExpr->pLeft
      && sqliteExprProcResolve(pParse, pBlock, pExpr->pLeft) ){
        return 1;
      }
      if( pExpr->pRight 
      && sqliteExprProcResolve(pParse, pBlock, pExpr->pRight) ){
        return 1;
      }
      if( pExpr->pList ){
        int i;
        ExprList *pList = pExpr->pList;
        for(i=0; i<pList->nExpr; i++){
          Expr *pArg = pList->a[i].pExpr;
          if( sqliteExprProcResolve(pParse, pBlock, pArg) ){
            return 1;
          }
        }
      }
    }
  }
  return 0;
}

int sqliteCompileSQLStmt(Parse *pParse, Block *b, SQLStmt* pSql){
  Vdbe *v = sqliteGetVdbe(pParse);
  int i,j;

  switch( pSql->op ){
  case TK_SELECT: {
	assert(pSql->pSelect);
	assert(pSql->pSelect->pSrc);
	sqliteSelect(pParse, pSql->pSelect, SRT_Stack, 0, 0, 0, 0);
	if( pSql->pExprList->nExpr!=pSql->pSelect->pEList->nExpr ) {
      sqliteErrorMsg(pParse, "INTO list does not match column list", 0);
	  return 1;
	}
	for(i=0; i<pSql->pExprList->nExpr; i++) {
	  Expr *e = pSql->pExprList->a[i].pExpr;
	  if( e->op!=TK_ID ) {
      sqliteErrorMsg(pParse, "Bad lvalue in INTO list", 0);
  		return 1;
	  }
      if( sqliteExprProcResolve(pParse, b, e) ){
        return 1;
	  }
 	  assert( e->op==TK_VAR );
	  if( e->flags==EP_NotNull ){
      j = sqliteVdbeMakeLabel(v);
      sqliteVdbeAddOp(v, OP_NotNull, -1, i);
	    sqliteVdbeOp3(v, OP_Halt, SQLITE_CONSTRAINT, OE_Abort,
                         "attempt to store null in non-null var", P3_STATIC);
      sqliteVdbeResolveLabel(v, j);
	  }
      sqliteVdbeAddOp(v, OP_MemStore, e->iColumn, 1);
	}
	break;
  }
  case TK_UPDATE: {
    SrcList *pSrc;
    pSrc = sqliteSrcListAppend(0, &pSql->target, 0);
    sqliteUpdate(pParse, pSrc, pSql->pExprList, pSql->pWhere, pSql->orconf);
    break;
  }
  case TK_INSERT: {
    SrcList *pSrc;
    pSrc = sqliteSrcListAppend(0, &pSql->target, 0);
    sqliteInsert(pParse, pSrc, pSql->pExprList, pSql->pSelect, pSql->pIdList, pSql->orconf);
    break;
  }
  case TK_DELETE: {
    SrcList *pSrc;
    pSrc = sqliteSrcListAppend(0, &pSql->target, 0);
    sqliteDeleteFrom(pParse, pSrc, pSql->pWhere);
    break;
  }
  default:
    assert(0);
  } 

  return 0;
}

static int sqliteCompileCall(
  Parse *pParse,
  Token *pName,
  ExprList *pEList
) {
  char *zName = 0;
  Vdbe *v = sqliteGetVdbe(pParse);
  Block *b = pParse->pCurrentBlock;
  Object * pObj = 0;
  sqlite *db = pParse->db;
  int i, nActual = 0;

  /* Check that the object exist & get its Object pointer*/
  zName = sqliteStrNDup(pName->z, pName->n);
  sqliteDequote(zName);
  pObj = sqliteHashFind(&(db->aDb[0].objectHash), zName,pName->n+1);
  if( !pObj ){
    sqliteErrorMsg(pParse, "object %T not found", pName);
    goto proc_cleanup;
  }
  if( pEList ) {
    nActual = pEList->nExpr;
  }
  if( pObj->nParam!=nActual ) {
  	sqliteErrorMsg(pParse, "bad parameter count for object %T", pName);
    goto proc_cleanup;
  }

  for(i=0; i<nActual; i++) {
	  Expr *pExpr = pEList->a[i].pExpr;
    if( sqliteExprProcResolve(pParse, b, pExpr) ){
      goto proc_cleanup;
    }
    if( sqliteExprCheck(pParse, pExpr, 0, 0) ){
      goto proc_cleanup;
    }
    sqliteExprCode(pParse, pExpr);
  }
  sqliteVdbeOp3(v, OP_Exec, nActual, 0, zName, P3_DYNAMIC);
  return 0;

proc_cleanup:
  sqliteFree(zName);
  return 1;
}

/* Hide a variable by setting its name to an empty string. This
** is used to hide the counter variable of a FOR loop at the end
** of the statement.
*/
static void hideVar(Block *b, int mVar) {
  int i;

  if( !b ) return;

  for( i=0; i<b->nVar; i++ ) {
	  if( b->aVar[i].mVar==mVar ) {
      b->aVar[i].zName[0] = 0;
      return;
    }
  }
}

static int sqliteCompileList(Parse*, Block*, StmtList*, int*, int);
static int sqliteCompileBlock(Parse*, Block*);

static int sqliteCompileStmt(
  Parse *pParse,    /* parse context */
  Block *b,         /* current block */
  Stmt* pStmt,      /* statement to compile */
  int *tailgoto,    /* set *tailgoto to 1 if last statement is a goto */
  int in_excep      /* set to 1 when compiling an exception handler */
){
  Vdbe *v = sqliteGetVdbe(pParse);
  SrcList dummy;
  int i, j, skipgoto = 0;

  dummy.nSrc = 0;

  if( tailgoto ) *tailgoto = 0;

  if( pStmt->op!=TK_RAISE && pStmt->op!=TK_PROCEDURE && pStmt->pExpr1 ){
    Expr *pExpr = pStmt->pExpr1;
    if( pStmt->op==TK_FOR ) {
      /* allocate the FOR counter variable (see case TK_FOR below) */
      sqliteAddProcVar(pParse, &(pExpr->pLeft->token));
    }
    if( sqliteExprProcResolve(pParse, b, pExpr) ){
      return 1;
    }
    if( sqliteExprCheck(pParse, pExpr, 0, 0) ){
      return 1;
    }
  }

  switch( pStmt->op ) {

  case TK_ASSIGN:{
    Expr *pLeft = pStmt->pExpr1->pLeft;
    Expr *pRight = pStmt->pExpr1->pRight;

    assert( pStmt->pExpr1->op==TK_ASSIGN );
	  assert( pLeft->op==TK_VAR );
    sqliteExprCode(pParse, pRight);
	  if( pLeft->flags==EP_NotNull ){
      i = sqliteVdbeMakeLabel(v);
      sqliteVdbeAddOp(v, OP_NotNull, -1, i);
	    sqliteVdbeOp3(v, OP_Halt, SQLITE_CONSTRAINT, OE_Abort,
                           "attempt to store null in non-null var", P3_STATIC);
      sqliteVdbeResolveLabel(v, i);
    }
    sqliteVdbeAddOp(v, OP_MemStore, pLeft->iColumn, 1);
	  break;
  }

  case TK_BLOCK:{
    if( sqliteCompileBlock(pParse, pStmt->pBlock) ){
      return 1;
    }
	  break;
  }

  case TK_CASE:{
    int jumpInst, addr;
    int nStmt;
    int searched;

    nStmt = pStmt->pStmt1->nStmt;
    searched = pStmt->pExpr1==0;
    assert( nStmt>0 );
    j = sqliteVdbeMakeLabel(v);
    if( !searched ){
      sqliteExprCode(pParse, pStmt->pExpr1);
    }
    for(i=0; i<nStmt; i++){
      Stmt *pWhen = pStmt->pStmt1->a[i].pStmt;
      assert( pWhen->op==TK_WHEN );
      if( sqliteExprProcResolve(pParse, b, pWhen->pExpr1) ){
        return 1;
      }
      if( sqliteExprCheck(pParse, pWhen->pExpr1, 0, 0) ){
        return 1;
      }
      sqliteExprCode(pParse, pWhen->pExpr1);
      if( !searched ){
        sqliteVdbeAddOp(v, OP_Dup, 1, 1);
        jumpInst = sqliteVdbeAddOp(v, OP_Ne, 1, 0);
      }else{
        jumpInst = sqliteVdbeAddOp(v, OP_IfNot, 1, 0);
      }
	    if( sqliteCompileList(pParse, b, pWhen->pStmt1, &skipgoto, 0) ){
	      return 1;
      }
      if( !skipgoto ) {
        sqliteVdbeAddOp(v, OP_Goto, 0, j);
      }
      addr = sqliteVdbeCurrentAddr(v);
      sqliteVdbeChangeP2(v, jumpInst, addr);
    }
    if( !searched ){
      sqliteVdbeAddOp(v, OP_Pop, 1, 0);
    }
    if( pStmt->pStmt2 ){
      assert( pStmt->pStmt2->op==TK_ELSE );
	    if( sqliteCompileList(pParse, b, pStmt->pStmt2->pStmt1, tailgoto, 0) ){
	      return 1;
      }
    }else{
      sqliteVdbeOp3(v, OP_Raise, 0, 0, "CASE_NOT_FOUND", P3_STATIC);
      if( tailgoto ) *tailgoto = 1;
    }
    sqliteVdbeResolveLabel(v, j);
    break;
  }

  case TK_EXIT:{
    if( pParse->iLoopExit==0 ) {
      sqliteErrorMsg(pParse, "EXIT used outside loop statement", 0);
      return 1;
    }
    if( pStmt->pExpr1 ) {
      sqliteExprCode(pParse, pStmt->pExpr1);
      sqliteVdbeAddOp(v, OP_If, 1, pParse->iLoopExit);
    } else {
      sqliteVdbeAddOp(v, OP_Goto, 0, pParse->iLoopExit);
      if( tailgoto ) *tailgoto = 1;
    }
	  break;
  }

  case TK_FOR:{
    Expr *pLow = pStmt->pExpr1->pRight->pLeft;
    Expr *pHigh = pStmt->pExpr1->pRight->pRight;
    int iCounter, iHigh, iPrevExit;

	  assert( pStmt->pExpr1->op==TK_ASSIGN );
	  assert( pStmt->pExpr1->pLeft->op==TK_VAR );
	  assert( pStmt->pExpr1->pRight->op==TK_FOR );
    iCounter = pParse->nMem-1;
    iHigh = pParse->nMem++;
    sqliteExprCode(pParse, pLow);
    sqliteVdbeAddOp(v, OP_MemStore, iCounter, 1);
    sqliteExprCode(pParse, pHigh);
    sqliteVdbeAddOp(v, OP_MemStore, iHigh, 1);
    sqliteVdbeAddOp(v, OP_MemLoad, iCounter, 0);
    i = sqliteVdbeCurrentAddr(v);
    sqliteVdbeAddOp(v, OP_MemLoad, iHigh, 0);
    iPrevExit = pParse->iLoopExit;
    pParse->iLoopExit = sqliteVdbeMakeLabel(v);
    sqliteVdbeAddOp(v, OP_Gt, 1, pParse->iLoopExit);
	  if( sqliteCompileList(pParse, b, pStmt->pStmt1, 0, 0) ){
	    return 1;
    }
    sqliteVdbeAddOp(v, OP_MemLoad, iCounter, 0);
    sqliteVdbeAddOp(v, OP_Integer, 1, 0);
    sqliteVdbeAddOp(v, OP_Add, 0, 0);
    sqliteVdbeAddOp(v, OP_MemStore, iCounter, 0);
    sqliteVdbeAddOp(v, OP_Goto, 0, i);
    sqliteVdbeResolveLabel(v, pParse->iLoopExit);
    pParse->iLoopExit = iPrevExit;
    hideVar(b, iCounter);
    break;
  }

  case TK_IF: {
    i = sqliteVdbeMakeLabel(v);
    j = sqliteVdbeMakeLabel(v);
    sqliteExprCode(pParse, pStmt->pExpr1);
    sqliteVdbeAddOp(v, OP_IfNot, 1, j);
	  if( sqliteCompileList(pParse, b, pStmt->pStmt1, &skipgoto, 0) ){
	    return 1;
    }
    while( pStmt->pStmt2 ) {
      if( !skipgoto ) {
        sqliteVdbeAddOp(v, OP_Goto, 0, i);
      }
      sqliteVdbeResolveLabel(v, j);
      j = sqliteVdbeMakeLabel(v);
      pStmt = pStmt->pStmt2;
      assert( pStmt->op==TK_ELSE || pStmt->op==TK_ELSIF );
      if( pStmt->op==TK_ELSIF ) {
        if( sqliteExprProcResolve(pParse, b, pStmt->pExpr1) ){
          return 1;
        }
        if( sqliteExprCheck(pParse, pStmt->pExpr1, 0, 0) ){
          return 1;
        }
        sqliteExprCode(pParse, pStmt->pExpr1);
        sqliteVdbeAddOp(v, OP_IfNot, 1, j);
      }
	    if( sqliteCompileList(pParse, b, pStmt->pStmt1, &skipgoto, 0) ){
	      return 1;
      }
    }
    sqliteVdbeResolveLabel(v, i);
    sqliteVdbeResolveLabel(v, j);
	  break;
  }

  case TK_LOOP:{
    int iPrevExit = pParse->iLoopExit;
    pParse->iLoopExit = sqliteVdbeMakeLabel(v);
    i = sqliteVdbeCurrentAddr(v);
	  if( sqliteCompileList(pParse, b, pStmt->pStmt1, 0, 0) ){
	    return 1;
    }
    sqliteVdbeAddOp(v, OP_Goto, 0, i);
    sqliteVdbeResolveLabel(v, pParse->iLoopExit);
    pParse->iLoopExit = iPrevExit;
	  break;
  }

  case TK_NULL:{
	  break;
  }

  case TK_PRINT:{
    sqliteExprCode(pParse, pStmt->pExpr1);
    sqliteVdbeAddOp(v, OP_Print, 0, 0);
	  break;
  }

  case TK_PROCEDURE: {
    Expr *pExpr = pStmt->pExpr1;
    if( sqliteCompileCall(pParse, &(pExpr->token), pExpr->pList) ) {
      return 1;
    }
    sqliteVdbeAddOp(v, OP_Pop, 1, 0);
	  break;
  }

  case TK_RAISE:{
    if( pStmt->pExpr1==0 ) {
      if( !in_excep ) {
        sqliteErrorMsg(pParse, "RAISE without argument illegal outside exception handler", 0);
        return 1;
      }
      sqliteVdbeOp3(v, OP_Raise, 0, 0, 0, P3_STATIC);
    } else {
      char *zName = 0;
      sqliteSetNString(&zName, pStmt->pExpr1->token.z, pStmt->pExpr1->token.n, 0);
      sqliteVdbeOp3(v, OP_Raise, 0, 0, zName, P3_DYNAMIC);
    }
    if( tailgoto ) *tailgoto = 1;
	  break;
  }

  case TK_RETURN:{
    sqliteExprCode(pParse, pStmt->pExpr1);
    sqliteVdbeAddOp(v, OP_MemStore, b->mReturn, 1);
    sqliteVdbeAddOp(v, OP_Goto, 0, b->nExit);
    if( tailgoto ) *tailgoto = 1;
	  break;
  }

  case TK_SQL:{
	  sqliteCompileSQLStmt(pParse, b, pStmt->pSql);
	  break;
  }

  case TK_WHILE:{
    int iPrevExit = pParse->iLoopExit;
    pParse->iLoopExit = sqliteVdbeMakeLabel(v);
    i = sqliteVdbeCurrentAddr(v);
    sqliteExprCode(pParse, pStmt->pExpr1);
    sqliteVdbeAddOp(v, OP_IfNot, 1, pParse->iLoopExit);
	  if( sqliteCompileList(pParse, b, pStmt->pStmt1, 0, 0) ){
	    return 1;
    }
    sqliteVdbeAddOp(v, OP_Goto, 0, i);
    sqliteVdbeResolveLabel(v, pParse->iLoopExit);
    pParse->iLoopExit = iPrevExit;
	  break;
  }

  }
  return 0;
}

static int sqliteCompileList(
  Parse *pParse,    /* parse context */
  Block *pBlock,    /* current block */
  StmtList *pList,  /* statements to compile */
  int *tailgoto,    /* set *tailgoto to 1 if last statment is a goto */
  int in_excep      /* set to 1 when compiling an exception handler */
){
  Vdbe *v = sqliteGetVdbe(pParse);
  int i, n = pList->nStmt;

  for(i=0; i<n; i++){
    if( sqliteCompileStmt(pParse, pBlock, pList->a[i].pStmt, tailgoto, in_excep) ){
      return 1;
    }
  }
  return 0;
}

static void sqliteOneHandler(Expr *pExpr, int lbl, int rev, Vdbe *v) {
  char *zExcep = sqliteStrNDup(pExpr->token.z, pExpr->token.n);

  assert( pExpr->op==TK_ID || pExpr->op==TK_STRING );
  sqliteDequote(zExcep);
  sqliteVdbeOp3(v, OP_ExcepWhen, rev, lbl, zExcep, P3_DYNAMIC);
}

static int sqliteCompileHandlers(Parse *pParse, Block *pBlock, StmtList *pExList){
  Vdbe *v = sqliteGetVdbe(pParse);
  int i, n = pExList->nStmt, skiphalt = 0;

  for(i=0; i<n; i++){
    Stmt *pEWhen = pExList->a[i].pStmt;
    Expr *pExpr = pEWhen->pExpr1;
    int lbl1, lbl2;

    assert( pEWhen->op==TK_WHEN );
    if( pExpr ) {
      lbl2 = sqliteVdbeMakeLabel(v);
      while( pExpr && pExpr->op==TK_OR ) {
        sqliteOneHandler( pExpr->pRight, lbl2, 1, v );
        pExpr = pExpr->pLeft;
      }
      lbl1 = sqliteVdbeMakeLabel(v);
      sqliteOneHandler( pExpr, lbl1, 0, v );
      sqliteVdbeResolveLabel(v, lbl2);
    } else {
      lbl1 = sqliteVdbeMakeLabel(v);
      sqliteVdbeOp3(v, OP_ExcepWhen, 0, lbl1, 0, P3_STATIC);
    }
    if( sqliteCompileList(pParse, pBlock, pEWhen->pStmt1, &skiphalt, 1) ){
      return 1;
    }
    if( !skiphalt ) {
      sqliteVdbeAddOp(v, OP_Goto, 0, pBlock->nExit);
    }
    sqliteVdbeResolveLabel(v, lbl1);
  }
  /* if no handler caught the exception, reraise it */
  sqliteVdbeOp3(v, OP_Raise, 0, 0, 0, P3_STATIC);
  return 0;
}

#include "vdbeInt.h"

static int sqliteCompileBlock(Parse *pParse, Block *b){
  Vdbe *v = sqliteGetVdbe(pParse);
  Block *saveCurBlock;
  int i, handler = 0;

  saveCurBlock = pParse->pCurrentBlock;
  pParse->pCurrentBlock = b;
  DbSetProperty(pParse->db, 0, DB_Cookie);

  b->nExit = sqliteVdbeMakeLabel(v);
	if( b->pExList ) {
		handler = sqliteVdbeMakeLabel(v);
    sqliteVdbeAddOp(v, OP_NewHandler, 0, handler);
	}
  for(i=0; i<b->nVar; i++) {
	  if( b->aVar[i].pDflt!=0 ) {
	    Expr *pExpr = b->aVar[i].pDflt;
      if( sqliteExprProcResolve(pParse, b, pExpr) ){
        return 1;
	    }
      if( sqliteExprCheck(pParse, pExpr, 0, 0) ){
        return 1;
	    }
      sqliteExprCode(pParse, pExpr);
      sqliteVdbeAddOp(v, OP_MemStore, b->aVar[i].mVar , 1);
    }
  }
  if( sqliteCompileList(pParse, b, b->pStList, 0, 0) ){
    return 1;
  }
  if( b->pExList ) {
    sqliteVdbeAddOp(v, OP_Goto, 0, b->nExit);
		sqliteVdbeResolveLabel(v, handler);
		if( sqliteCompileHandlers(pParse, b, b->pExList) ){
      return 1;
		}
  }
  sqliteVdbeResolveLabel(v, b->nExit);
	if( b->pExList && b->pParent!=0 ) {
    sqliteVdbeAddOp(v, OP_PrevHandler, 0, 0);
	}
  /* if we end with 'goto next' (last stmt is a return), remove it */
//  if( v->aOp[v->nOp-1].opcode==OP_Goto && v->aOp[v->nOp-1].p2==v->nOp ) {
//    v->nOp--;
//  }
  pParse->pCurrentBlock = saveCurBlock;
  return 0;
}

/*************************************************************************
** 
** Routines to assist in building / compiling CREATE PROCEDURE, FUNCTION
** and DROP PROCEDURE, FUNCTION
*/

void sqliteBeginProc(
  Parse *pParse,      /* The parse context of the statement */
  int what,           /* One of TK_PROCEDURE or TK_FUNCTION */
  Token *pName        /* The name of the object */
){
  Object *no;
  Block *pBlock = pParse->pCurrentBlock;
  char *zName = 0;        /* Name of the object */
  sqlite *db = pParse->db;

  /* Check that the object name does not already exist */
  zName = sqliteStrNDup(pName->z, pName->n);
  sqliteDequote(zName);
  if( !pParse->explain &&
	  sqliteHashFind(&(db->aDb[0].objectHash), zName,pName->n+1) ){
    sqliteErrorMsg(pParse, "object %T already exists", pName);
    goto object_cleanup;
  }
  /* Build the object */
  no = (Object*)sqliteMalloc(sizeof(Object));
  if( no==0 ) goto object_cleanup;
  no->name = zName;
  zName = 0;
  no->what = what;
  no->iDb = 0;
  no->nParam = pBlock->nVar;
  /* add param checks here */
  pBlock->pObj = no;
  pBlock->params = 0;
  assert( pParse->pNewTrigger==0 );
  pParse->pNewObject = no;
  return;

object_cleanup:
  sqliteFree(zName);
}

void sqliteDeleteObject(Object *pObject) {
  int i;

  if( pObject==0 ) return;
  sqliteFree(pObject->name);
  for(i=0; i<pObject->nOp; i++){
    if( pObject->aOp[i].p3type==P3_DYNAMIC ){
      sqliteFree(pObject->aOp[i].p3);
    }
  }
  sqliteFree(pObject->aOp);
  sqliteFree(pObject);
}

/*
** This routine is called after the body of the procedure has been parsed
** in order to complete the process of building the procedure object.
*/
void sqliteFinishProc(
  Parse *pParse,          /* Parser context */
  Block *pBlock,		  /* The procedure body */
  Token *pAll             /* Token that describes the complete CREATE text */
){
  Object *no = 0;           /* The object whose construction is finishing up */
  sqlite *db = pParse->db;  /* The database */
  Vdbe *v = sqliteGetVdbe(pParse);

  if( pParse->nErr || pParse->pNewObject==0 ) goto objectfinish_cleanup;
  no = pParse->pNewObject;
  pParse->pNewObject = 0;

  sqliteCompileBlock(pParse, pBlock);
  sqliteVdbeAddOp(v, OP_Halt, 0, 0);

  /* save compiled body code, reset vdbe */
  if( !pParse->explain ){
    no->nOp = v->nOp;
    v->nOp = 0;
    no->aOp = v->aOp;
    v->aOp = 0;
    v->nOpAlloc = 0;
  }
  DbClearProperty(db, 0, DB_Locked);
  DbClearProperty(db, 1, DB_Locked);

  /* if we are not initializing build the sqlite_master entry */
  if( !db->init.busy ){
    static VdbeOpList insertObj[] = {
      { OP_NewRecno,   0, 0,  0           },
      { OP_String,     0, 0,  "procedure" },
      { OP_String,     0, 0,  0           },  /* 2: object name */
      { OP_String,     0, 0,  0           },  
      { OP_Integer,    0, 0,  0           },
      { OP_String,     0, 0,  0           },  /* 5: SQL */
      { OP_MakeRecord, 5, 0,  0           },
      { OP_PutIntKey,  0, 0,  0           },
    };
    int addr;

    /* Make an entry in the sqlite_master table */
    if( v==0 ) goto objectfinish_cleanup;
    sqliteBeginWriteOperation(pParse, 0, 0);
    sqliteOpenMasterTable(v, 0);
    addr = sqliteVdbeAddOpList(v, ArraySize(insertObj), insertObj);
    sqliteVdbeChangeP3(v, addr+2, no->name, 0); 
    sqliteVdbeChangeP3(v, addr+5, pAll->z, pAll->n);
    if( no->iDb==0 ){
      sqliteChangeCookie(db, v);
    }
    sqliteVdbeAddOp(v, OP_Close, 0, 0);
    sqliteEndWriteOperation(pParse);
  }

  if( !pParse->explain ){
    sqliteHashInsert(&db->aDb[no->iDb].objectHash, 
                     no->name, strlen(no->name)+1, no);
    no = 0;
  }

objectfinish_cleanup:
  sqliteDeleteObject(no);
  sqliteDeleteObject(pParse->pNewObject);
  pParse->pNewObject = 0;
}

void sqliteDropProc(Parse *pParse, Token *pName){
  Object *pObj;
  char *zName;
  Vdbe *v = sqliteGetVdbe(pParse);
  sqlite *db = pParse->db;

  zName = sqliteStrNDup(pName->z, pName->n);
  sqliteDequote(zName);
  pObj = sqliteHashFind(&(db->aDb[0].objectHash), zName, pName->n+1);
  if( !pParse->explain && !pObj ){
    sqliteErrorMsg(pParse, "no such object: %T", pName);
    goto dropobject_cleanup;
  }

  /* Generate code to destroy the database record of the trigger.
  */
  if( v ){
    int base;
    static VdbeOpList dropObject[] = {
      { OP_Rewind,     0, ADDR(9),  0},
      { OP_String,     0, 0,        0}, /* 1 */
      { OP_Column,     0, 1,        0},
      { OP_Ne,         0, ADDR(8),  0},
      { OP_String,     0, 0,        "procedure"},
      { OP_Column,     0, 0,        0},
      { OP_Ne,         0, ADDR(8),  0},
      { OP_Delete,     0, 0,        0},
      { OP_Next,       0, ADDR(1),  0}, /* 8 */
    };

    sqliteBeginWriteOperation(pParse, 0, 0);
    sqliteOpenMasterTable(v, 0);
    base = sqliteVdbeAddOpList(v,  ArraySize(dropObject), dropObject);
    sqliteVdbeChangeP3(v, base+1, zName, 0);
    if( pObj && pObj->iDb==0 ){
      sqliteChangeCookie(db, v);
    }
    sqliteVdbeAddOp(v, OP_Close, 0, 0);
    sqliteEndWriteOperation(pParse);
  }

  /*
   * If this is not an "explain", then delete the trigger structure.
   */
  if( !pParse->explain ){
    sqliteHashInsert(&(db->aDb[pObj->iDb].objectHash), zName, pName->n+1, 0);
    sqliteDeleteObject(pObj);
  }

dropobject_cleanup:
  sqliteFree(zName);
}

/*************************************************************************
** 
** Routines to assist in building / compiling EXEC commands
*/

void sqliteExecProc(Parse *pParse, Token *pName, ExprList *pEList) {
  Vdbe *v = sqliteGetVdbe(pParse);

  sqliteVdbeOp3(v, OP_ColumnName, 0, 1, "Result", P3_STATIC);
  if( sqliteCompileCall(pParse, pName, pEList) ) {
    return;
  }
  sqliteVdbeAddOp(v, OP_Callback, 1, 0);
  sqliteVdbeAddOp(v, OP_Halt, 0, 0);
  return;
}

void sqliteExecBlock(Parse *pParse, Block *pBlock) {
  Vdbe *v = sqliteGetVdbe(pParse);

  sqliteVdbeOp3(v, OP_ColumnName, 0, 1, "Result", P3_STATIC);
  sqliteVdbeAddOp(v, OP_String, 0, 0);
  sqliteVdbeAddOp(v, OP_MemStore, 0, 1);
  sqliteCompileBlock(pParse, pBlock);
  sqliteVdbeAddOp(v, OP_MemLoad, 0, 0);
  sqliteVdbeAddOp(v, OP_Callback, 1, 0);
  sqliteVdbeAddOp(v, OP_Halt, 0, 0);

  sqliteBlockDelete(pBlock);
}
