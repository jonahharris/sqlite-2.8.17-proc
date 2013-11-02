```sql
CREATE TABLE mytable (myval INTEGER);

CREATE PROCEDURE inslist(i integer) AS
BEGIN
  WHILE i>0 LOOP
    INSERT INTO mytable VALUES (i);
    i := i - 1;
  END LOOP;
  RETURN 'ok';
END;

CREATE PROCEDURE addlist(i integer) AS
DECLARE
  v_max INTEGER;
BEGIN
  SELECT MAX(myval)
    INTO v_max
    FROM mytable;

  WHILE i > 0 LOOP
    INSERT INTO mytable VALUES (i + v_max);
    i := i - 1;
  END LOOP;
  RETURN 'ok';
END;

-- Test inline procedure
EXEC
BEGIN
  inslist(50);
END;
select (50 = max(myval)) from mytable;

-- Test calling procedure
call addlist(50);
select (100 = max(myval)) from mytable;
```
