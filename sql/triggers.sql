CREATE OR REPLACE FUNCTION create_trigger_stamp(_tbl regclass, _col  text, _action text) RETURNS void AS $$
DECLARE
    _trigger_name text;
BEGIN
    IF EXISTS (SELECT 1 FROM pg_attribute
        WHERE  attrelid = _tbl
        AND    attname = _col
        AND    NOT attisdropped) THEN
            _trigger_name := _tbl || '_' || _action || '_stamp';
            EXECUTE 'CREATE FUNCTION ' || _trigger_name || '() RETURNS trigger AS $' || _trigger_name || '$
            BEGIN
                NEW.' || _col || ' := current_timestamp;
                RETURN NEW;
            END;
            $' || _trigger_name || '$ LANGUAGE plpgsql;';
            EXECUTE 'ALTER FUNCTION ' || _trigger_name || '() OWNER TO madison;';
            EXECUTE 'CREATE TRIGGER ' || _trigger_name || ' BEFORE ' || _action || ' ON ' || _tbl ||
                    ' FOR EACH ROW EXECUTE PROCEDURE '|| _trigger_name || '();';
    END IF;
END
$$  LANGUAGE plpgsql;

ALTER FUNCTION create_trigger_stamp(regclass, text, text) OWNER TO madison;

CREATE OR REPLACE FUNCTION check_for_stamps()
  RETURNS event_trigger
  AS $$
DECLARE
    record RECORD;
BEGIN
    FOR record IN SELECT * FROM pg_event_trigger_ddl_commands() LOOP
        IF record.command_tag = 'CREATE TABLE'
        THEN
            PERFORM create_trigger_stamp(record.object_identity, 'created_at', 'INSERT');
            PERFORM create_trigger_stamp(record.object_identity, 'updated_at', 'UPDATE');
        END IF;
    END LOOP;
END;
$$
LANGUAGE plpgsql;

ALTER FUNCTION check_for_stamps() OWNER TO madison;

CREATE EVENT TRIGGER check_trigger_stamps ON ddl_command_end
   EXECUTE PROCEDURE check_for_stamps();

CREATE OR REPLACE FUNCTION clean_stamps()
  RETURNS event_trigger
  AS $$
DECLARE
    record RECORD;
BEGIN
    FOR record IN SELECT * FROM pg_event_trigger_dropped_objects() LOOP
        IF record.object_type = 'table'
        THEN
            EXECUTE 'DROP FUNCTION IF EXISTS ' || record.object_identity || '_insert_stamp();';
            EXECUTE 'DROP FUNCTION IF EXISTS ' || record.object_identity || '_update_stamp();';
        END IF;
    END LOOP;
END;
$$
LANGUAGE plpgsql;

ALTER FUNCTION clean_stamps() OWNER TO madison;

CREATE EVENT TRIGGER clean_trigger_stamps ON sql_drop
   EXECUTE PROCEDURE clean_stamps();
