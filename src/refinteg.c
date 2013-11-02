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
** This file contains routines used for generating referential integrity
** triggers.
**
** 
*/
#include "sqliteInt.h"

/*
** A simple string class to do efficient appends
*/
struct riStr {
	int  nAlloc;
	int  nStr;
	char *zStr;
};
typedef struct riStr riStr;

#define EXTEND_SIZE 200

static riStr *alloc_str() {
  riStr *s = sqliteMalloc(sizeof(riStr));
	s->nAlloc = EXTEND_SIZE;
	s->zStr = sqliteMalloc(s->nAlloc);
	s->zStr[0] = 0;
	s->nStr = 0;
	return s;
}

static void clr_str(riStr *pStr) {
	if( !pStr ) return;
	pStr->zStr[0] = 0;
	pStr->nStr = 0;
}

static void free_str(riStr *pStr) {
	if( !pStr ) return;
	sqliteFree( pStr->zStr );
	sqliteFree( pStr );
}

static void print_str(riStr *pStr, char *zAddStr) {
	int addLen = strlen(zAddStr);
	if( pStr->nStr + addLen >= pStr->nAlloc ) {
		int n = pStr->nAlloc + addLen + EXTEND_SIZE;
		pStr->zStr = sqliteRealloc(pStr->zStr, n);
		pStr->nAlloc = n;
	}
	pStr->nStr += sprintf(pStr->zStr+pStr->nStr, "%s", zAddStr);
}

static void print_lst(riStr *pStr, ...) {
	char *zText;
  va_list ap;

  va_start(ap,pStr);
	while( (zText = va_arg(ap,char *)) ) {
	  print_str(pStr, zText);
	}
	va_end(ap);
}

static void trunc_str(riStr *pStr, int n) {
	assert( pStr->nStr >= n );
	pStr->nStr -= n;
	pStr->zStr[pStr->nStr] = 0;
}

/*
** Generate the referential integrity triggers for one foreign key constraint.
** If the "To" table does not exist at this point in time (COMMIT), the
** foreign key constraint is ignored with a warning.
*/

static int gen_constraint(sqlite *db, FKey *pFKey, char **pzErr) {
  char *zFrom = pFKey->pFrom->zName;
	char *zTo   = pFKey->zTo;
	char zTrigger[100]; /* trigger name */
	char zConstr[100];  /* constraint name */
	Table *pTo;
	riStr *trig;
	int  i, rc, iDb;

	assert(pFKey->insertConf==OE_Restrict);

	/* Check that the "To" table exists, ignore this FK otherwise. Also check,
	** if a primary key is implied, that such key actually exists. If either
	** check fails, processing continues, but the foreign key constraint is
	** further ignored.
	*/
	iDb = pFKey->pFrom->iDb;
	pTo = sqliteFindTable(db, zTo, (iDb==1) ? (char*)0 : db->aDb[iDb].zName);
	if( pTo==0 ) {
		sqliteSetString(pzErr, "foreign key on table ", zFrom,
			" references non-existent table ", zTo, (char*)0);
		return SQLITE_OK;
	}
	if( pFKey->nCol==1 && pFKey->aCol[0].zCol==0 ) {
		for(i=0; i<pTo->nCol; i++) {
			if( pTo->aCol[i].isPrimKey ) break;
		}
		if( i==pTo->nCol ) {
			sqliteSetString(pzErr, "foreign key on table ", zFrom,
				" uses non-existent primary key on table ", zTo, (char*)0);
			return SQLITE_OK;
		}
	  pFKey->aCol[0].zCol = pTo->aCol[i].zName;
	}

	/* Generate the names. TODO: handle user named constraints */
	sprintf(zTrigger, "sqlite_%d_%d_%d", pFKey->pFrom->iDb, pFKey->pFrom->tnum, pFKey->nId);
  sprintf(zConstr, "%s_%d", zFrom, pFKey->nId);

	/* Handle the child table constraints. If the matching row in the foreign
	** table is not found, raise an error.
	*/
  trig = alloc_str();
	print_lst(trig, "CREATE TRIGGER ", zTrigger, "_i BEFORE INSERT ON ", zFrom, (char*)0);
	print_lst(trig, " FOR EACH ROW BEGIN", (char*)0);
	print_lst(trig, " SELECT CASE WHEN ((SELECT ROWID FROM ", zTo, " WHERE ", (char*)0);
  for(i=0; i<pFKey->nCol; i++) {
		print_lst(trig, pFKey->aCol[i].zCol, "=NEW.", (char*)0);
		print_lst(trig, pFKey->pFrom->aCol[pFKey->aCol[i].iFrom].zName, " AND ", (char*)0);
	}
	trunc_str(trig, 5);
	print_lst(trig, ") IS NULL) THEN RAISE(ABORT, 'INSERT on table \"", zFrom, (char*)0);
	print_lst(trig, "\" violates foreign key constraint \"", zConstr, (char*)0);
	print_str(trig, "\"') END; END;");
//printf("%s\n", trig->zStr);
	rc = sqlite_exec(db, trig->zStr, 0, 0, pzErr);
  if( rc!=SQLITE_OK ) goto gen_abort;
	clr_str(trig);

	print_lst(trig, "CREATE TRIGGER ", zTrigger, "_u BEFORE UPDATE ON ", zFrom, (char*)0);
	print_lst(trig, " FOR EACH ROW BEGIN", (char*)0);
	print_lst(trig, " SELECT CASE WHEN ((SELECT ROWID FROM ", zTo, " WHERE ", (char*)0);
  for(i=0; i<pFKey->nCol; i++) {
		print_lst(trig, pFKey->aCol[i].zCol, "=NEW.", (char*)0);
		print_lst(trig, pFKey->pFrom->aCol[pFKey->aCol[i].iFrom].zName, " AND ", (char*)0);
	}
	trunc_str(trig, 5);
	print_lst(trig, ") IS NULL) THEN RAISE(ABORT, 'INSERT on table \"", zFrom, (char*)0);
	print_lst(trig, "\" violates foreign key constraint \"", zConstr, (char*)0);
	print_str(trig, "\"') END; END;");
//printf("%s\n", trig->zStr);
	rc = sqlite_exec(db, trig->zStr, 0, 0, pzErr);
  if( rc!=SQLITE_OK ) goto gen_abort;
	clr_str(trig);

	/* Handle the ON DELETE constraint. RESTRICT (the default) raises an error if the
	** parent row still has dependencies, SET NULL and SET DEFAULT cause the dependent
	** rows to be updated accordingly and CASCADE deletes the dependent rows
	*/
	print_lst(trig, "CREATE TRIGGER ", zTrigger, "_d BEFORE DELETE ON ", zTo,
		                     " FOR EACH ROW BEGIN ", (char*)0);
	switch(pFKey->deleteConf) {
		case OE_Restrict: {
			print_lst(trig, "SELECT CASE WHEN ((SELECT ROWID FROM ", zFrom, " WHERE ", (char*)0);
			for(i=0; i<pFKey->nCol; i++) {
				print_lst(trig, "OLD.", pFKey->aCol[i].zCol, "=", (char*)0);
				print_lst(trig, pFKey->pFrom->aCol[pFKey->aCol[i].iFrom].zName, " AND ", (char*)0);
			}
			trunc_str(trig, 5);
			print_lst(trig, ") IS NOT NULL) THEN RAISE(ABORT, 'DELETE on table \"", zTo, (char*)0);
			print_lst(trig, "\" violates foreign key constraint \"", zConstr, (char*)0);
			print_str(trig, "\"') END; END;");
			break;
		}
		case OE_SetNull:
		case OE_SetDflt: {
			print_lst(trig, "UPDATE ", zFrom, " SET ", (char*)0);
			for(i=0; i<pFKey->nCol; i++) {
				print_str(trig, pFKey->pFrom->aCol[pFKey->aCol[i].iFrom].zName);
				switch(pFKey->deleteConf) {
				  case OE_SetDflt: {
						if( pTo->aCol[i].zDflt ) {
							print_lst(trig, "=", pTo->aCol[i].zDflt, (char*)0);
							break;
						}
					}
					case OE_SetNull: print_str(trig, "=NULL, "); break;
				}
			}
			trunc_str(trig, 2);
			print_str(trig, " WHERE ");
			for(i=0; i<pFKey->nCol; i++) {
				print_lst(trig, "OLD.", pFKey->aCol[i].zCol, "=", (char*)0);
				print_lst(trig, pFKey->pFrom->aCol[pFKey->aCol[i].iFrom].zName, " AND ", (char*)0);
			}
			trunc_str(trig, 5);
			print_str(trig, "; END;");
			break;
		}
		case OE_Cascade: {
			print_lst(trig, "DELETE FROM ", zFrom, " WHERE ", (char*)0);
			for(i=0; i<pFKey->nCol; i++) {
				print_lst(trig, "OLD.", pFKey->aCol[i].zCol, "=", (char*)0);
				print_lst(trig, pFKey->pFrom->aCol[pFKey->aCol[i].iFrom].zName, " AND ", (char*)0);
			}
			trunc_str(trig, 5);
			print_str(trig, "; END;");
			break;
		}
	}
//printf("%s\n", trig->zStr);
  rc = sqlite_exec(db, trig->zStr, 0, 0, pzErr);
  if( rc!=SQLITE_OK ) goto gen_abort;
	clr_str(trig);

	/* Handle the ON DELETE constraint. RESTRICT (the default) raises an error if the
	** parent row still has dependencies, SET NULL and SET DEFAULT cause the dependent
	** rows to be updated accordingly and CASCADE propagates the update.
	*/
	print_lst(trig, "CREATE TRIGGER ", zTrigger, "_v BEFORE UPDATE ON ", zTo,
		                     " FOR EACH ROW BEGIN ", (char*)0);
	switch(pFKey->updateConf) {
		case OE_Restrict: {
			print_lst(trig, "SELECT CASE WHEN ((SELECT ROWID FROM ", zFrom, " WHERE ", (char*)0);
			for(i=0; i<pFKey->nCol; i++) {
				print_lst(trig, "OLD.", pFKey->aCol[i].zCol, "=", (char*)0);
				print_lst(trig, pFKey->pFrom->aCol[pFKey->aCol[i].iFrom].zName, " AND ", (char*)0);
			}
			trunc_str(trig, 5);
			print_lst(trig, ") IS NOT NULL) THEN RAISE(ABORT, 'UPDATE on table \"", zTo, (char*)0);
			print_lst(trig, "\" violates foreign key constraint \"", zConstr, (char*)0);
			print_str(trig, "\"') END; END;");
			break;
		}
		case OE_SetNull:
		case OE_SetDflt:
		case OE_Cascade: {
			print_lst(trig, "UPDATE ", zFrom, " SET ", (char*)0);
			for(i=0; i<pFKey->nCol; i++) {
				print_str(trig, pFKey->pFrom->aCol[pFKey->aCol[i].iFrom].zName);
				switch(pFKey->updateConf) {
				  case OE_SetDflt: {
						if( pTo->aCol[i].zDflt ) {
							print_lst(trig, "=", pTo->aCol[i].zDflt, (char*)0);
							break;
						}
					}
					case OE_SetNull: print_str(trig, "=NULL, "); break;
					case OE_Cascade: print_lst(trig, "=NEW.", pFKey->aCol[i].zCol, ", ", (char*)0); break;
				}
			}
			trunc_str(trig, 2);
			print_str(trig, " WHERE ");
			for(i=0; i<pFKey->nCol; i++) {
				print_lst(trig, "OLD.", pFKey->aCol[i].zCol, "=", (char*)0);
				print_lst(trig, pFKey->pFrom->aCol[pFKey->aCol[i].iFrom].zName, " AND ", (char*)0);
			}
			trunc_str(trig, 5);
			print_str(trig, "; END;");
			break;
		}
	}
//printf("%s\n", trig->zStr);
  rc = sqlite_exec(db, trig->zStr, 0, 0, pzErr);
  if( rc!=SQLITE_OK ) goto gen_abort;

	free_str(trig);
	return SQLITE_OK;

gen_abort:
	free_str(trig);
	return rc;
}

/*
** Generate the triggers to enforce integrity. This function is called
** just before COMMIT, when db->pDeferredFKeys is not null. It even comes
** before the xCommitCallback function. See OP_Commit in vdbe.c for details.
** It is also called by the sqliteInitOne routine, whenever the schema is
** reloaded (on startup and after a schema change).
**
** Set db->init.busy to true, so that we enter the constraint trigger into
** the cache, but do not enter it into the sqlite_master table.
*/

int sqliteDoDefDDL(sqlite *db, char **pzErr) {
	FKey *pFKey = db->pDeferredFKeys;
	int rc, busy;

  busy = db->init.busy;
	db->init.busy = 1;
 	db->pDeferredFKeys = 0;
	while( pFKey ) {
		if( (rc=gen_constraint(db, pFKey, pzErr))!=SQLITE_OK ) {
		  db->init.busy = busy;
			return rc;
		}
		pFKey = pFKey->pNextDef;
	}
  db->init.busy = busy;
	return SQLITE_OK;
}
