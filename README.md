# Tampa Bay Sun FC — Tooling

Automated data pipeline and playoff modeling for Tampa Bay Sun FC (USL Super League). Runs on a TrueNAS home server via Docker, writes to a local PostgreSQL database.

---

## Repository Structure

### R Scripts

| File | Role |
|------|------|
| `tampa_bay_sun_roster_scraper.R` | Scrapes the USLSL roster page, detects changes vs. the database, and sends a Discord/email notification when players are added or dropped |
| `playoff_modeling.R` | Interactive modeling script for local use — runs Monte Carlo simulations (1M sims) to calculate playoff probabilities for all 9 USL Super League teams, produces visualizations |
| `playoff_runner.R` | Production version of `playoff_modeling.R` — sources functions from that file, reads config from environment variables, and writes results to Postgres instead of plotting |
| `progress.R` | Scratch/exploratory script |
| `test_scraper.R` | Scratch/testing script |

### Docker — Roster Scraper

| File | Role |
|------|------|
| `Dockerfile` | Builds the roster scraper image based on `rocker/r-base` |
| `docker-entrypoint.sh` | Exports Docker environment variables to `/tmp/docker.env` so cron jobs can read them |
| `startup.sh` | Container entrypoint — optionally runs the scraper on startup, then starts cron |
| `run_scraper.sh` | Shell script invoked by cron — clones the latest repo, copies the scraper script, and runs it |

### Docker — Playoff Runner

| File | Role |
|------|------|
| `Dockerfile.runner` | Builds the playoff runner image based on `rocker/r-base` — installs simulation dependencies and sets a Mon/Thu 3 AM cron |
| `startup_playoff.sh` | Container entrypoint — optionally runs the simulation on startup, then starts cron |
| `run_playoff_simulation.sh` | Shell script invoked by cron — clones the latest repo, copies `playoff_runner.R`, and runs it |

### Infrastructure

| File | Role |
|------|------|
| `docker-compose.yml` | Defines both Docker services (`tb_sun` roster scraper and `playoff-runner`) with environment variable wiring |
| `.github/workflows/docker-build.yml` | GitHub Actions workflow — builds and pushes both Docker images to GHCR on push to `main` |

### Docs

| File | Role |
|------|------|
| `playoff_shiny_setup.md` | Step-by-step guide for the full playoff pipeline setup: Postgres schema, runner deployment, and Shiny app connection |
| `roster_change_detection_postgres_discord.md` | Notes on the roster change detection design |

---

## Architecture

```
GitHub Actions
    │
    ├─ builds ghcr.io/ryanscharf/tb_sun (roster scraper)
    └─ builds ghcr.io/ryanscharf/tb_sun-playoff-runner

TrueNAS Server (192.168.2.66)
    ├─ tb_sun container          → scrapes roster → PostgreSQL
    └─ playoff-runner container  → runs simulation → PostgreSQL
                                        │
                              Cloudflare Tunnel
                              pg-tn.ryanscharf.com
                                        │
                                   Shiny App
                           (shinyapps.io / Posit Connect)
```

## Environment Variables

Both containers are configured via `.env` (not committed). Key variables:

| Variable | Used By | Description |
|----------|---------|-------------|
| `DB_HOST` | both | Postgres host (`192.168.2.66`) |
| `DB_PORT` | both | Postgres port (`5432`) |
| `DB_NAME` | both | Database name (`tb_sun`) |
| `DB_USERNAME` | both | Write-access user |
| `DB_PASSWORD` | both | Write-access password |
| `N_SIMS` | playoff-runner | Number of Monte Carlo simulations (default `1000000`) |
| `N_CORES` | playoff-runner | Parallel workers (default `16`) |
| `RUN_ON_STARTUP` | roster scraper | Run scraper when container starts |
| `PLAYOFF_RUN_ON_STARTUP` | playoff-runner | Run simulation when container starts |
