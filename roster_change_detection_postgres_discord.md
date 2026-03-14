# Roster Change Detection (Postgres + Discord)

This document describes how to detect roster changes in Postgres and push real‑time notifications to a Discord bot using **snapshots, diffs, triggers, and LISTEN / NOTIFY**.

The system assumes:
- A scraper inserts roster rows into Postgres on each run
- Each scrape produces a new `ao_datetime` timestamp
- Player identity is `(number, name)`

---

## Overview

Flow:

```
Scraper inserts roster snapshot
            ↓
Stored procedure diffs latest vs previous snapshot
            ↓
Added/removed players inserted into roster_events
            ↓
Trigger fires pg_notify()
            ↓
Rust Discord bot LISTENs and posts
```

Postgres acts as both:
- Source of truth (durable event log)
- Message broker (real‑time notifications)

---

## Core Tables

### Roster Snapshots (`roster`)

Must already exist, with at least:

```sql
number INT
name TEXT
ao_datetime TIMESTAMPTZ
```

Each scrape appends a new batch of rows with a new `ao_datetime`.

---

### Roster Events (`roster_events`)

Stores detected adds/removes.

```sql
CREATE TABLE IF NOT EXISTS roster_events (
  id BIGSERIAL PRIMARY KEY,
  event_type TEXT CHECK (event_type IN ('added','removed')),
  number INT,
  name TEXT,
  ao_datetime TIMESTAMPTZ,
  event_time TIMESTAMPTZ DEFAULT now()
);

CREATE UNIQUE INDEX IF NOT EXISTS roster_events_dedupe
ON roster_events (event_type, number, name, ao_datetime);
```

Purpose:
- Durable event history
- Prevent duplicate Discord messages
- Replay missed notifications

---

## Stored Procedure: Detect Roster Changes

Diffs the latest snapshot against the previous snapshot.

```sql
CREATE OR REPLACE FUNCTION detect_roster_changes()
RETURNS void
LANGUAGE plpgsql
AS $$
DECLARE
  t_new TIMESTAMPTZ;
  t_old TIMESTAMPTZ;
BEGIN
  SELECT ao_datetime INTO t_new
  FROM roster
  ORDER BY ao_datetime DESC
  LIMIT 1;

  SELECT ao_datetime INTO t_old
  FROM roster
  WHERE ao_datetime < t_new
  ORDER BY ao_datetime DESC
  LIMIT 1;

  IF t_old IS NULL THEN
    RETURN;
  END IF;

  INSERT INTO roster_events (event_type, number, name, ao_datetime)
  SELECT 'added', number, name, t_new
  FROM roster
  WHERE ao_datetime = t_new
  EXCEPT
  SELECT 'added', number, name, t_new
  FROM roster
  WHERE ao_datetime = t_old;

  INSERT INTO roster_events (event_type, number, name, ao_datetime)
  SELECT 'removed', number, name, t_old
  FROM roster
  WHERE ao_datetime = t_old
  EXCEPT
  SELECT 'removed', number, name, t_old
  FROM roster
  WHERE ao_datetime = t_new;
END;
$$;
```

### Run After Each Scrape

```sql
SELECT detect_roster_changes();
```

---

## Real‑Time Notifications (Trigger + NOTIFY)

### Trigger Function

```sql
CREATE OR REPLACE FUNCTION notify_roster_change()
RETURNS trigger
LANGUAGE plpgsql
AS $$
BEGIN
  PERFORM pg_notify(
    'roster_changes',
    json_build_object(
      'event_type', NEW.event_type,
      'number', NEW.number,
      'name', NEW.name,
      'ao_datetime', NEW.ao_datetime,
      'event_time', NEW.event_time
    )::text
  );
  RETURN NEW;
END;
$$;
```

### Trigger Hook

```sql
DROP TRIGGER IF EXISTS roster_change_notify ON roster_events;

CREATE TRIGGER roster_change_notify
AFTER INSERT ON roster_events
FOR EACH ROW
EXECUTE FUNCTION notify_roster_change();
```

---

## How LISTEN / NOTIFY Works

### In Postgres

- `pg_notify(channel, payload)` sends a message
- Messages are delivered only to active listeners
- Notifications send **only after transaction commit**

### In the Rust Bot

```sql
LISTEN roster_changes;
```

The bot waits on the connection socket and receives JSON messages instantly.

---

## Reliability Guarantees

| Feature | Behavior |
|--------|--------|
| Event storage | Durable in `roster_events` |
| Real‑time push | Via NOTIFY |
| Bot offline | Events preserved in DB |
| Duplicate prevention | Unique index |
| Crash safety | Transaction‑safe |

Recommended pattern:
1. On startup, bot polls unsent events
2. Then switches to LISTEN mode

---

## Example Event Payload

```json
{
  "event_type": "added",
  "number": 12,
  "name": "Player Name",
  "ao_datetime": "2026‑01‑26T20:00:00Z",
  "event_time": "2026‑01‑26T20:00:01Z"
}
```

---

## Notes & Future Improvements

- Identity can later be upgraded to a stable player ID
- Position/team metadata can be added to payload
- Additional channels (Slack, email) can consume the same events

---

## Summary

This system provides:
- Snapshot‑based roster tracking
- Accurate add/remove detection
- Durable audit history
- Real‑time Discord notifications
- Minimal coupling between scraper and bot

Postgres acts as both the **diff engine** and the **event bus**, keeping the architecture simple and reliable.

