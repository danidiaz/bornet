# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

The project consists of two Cabal packages organized as a multi-package project:
- `bornet`: Main application, containing the model and api.
- `dani-beans`: A git submodule containing infrastructure libraries, not related to the model.

## Commands

To build the application:

- cabal build bornet

## Database Management

The SQLite database (`bornet/db.sqlite`) is managed through SQL scripts in `bornet/sql/`:
- `schema.sql`: Database schema definition
- `data.sql`: Base data
- `horario_inserts.sql`: Extra, more voluminous, base data.

The `init_data.sh` script recreates the database from scratch by running these scripts in order.

## Architecture

### Dependency Injection with Cauldron

The application uses the `cauldron` library for dependency injection and resource management. The dependency graph is defined in `Bornet.Root.cauldron` where all components are wired together using recipes.

Key pattern: Components are declared with `recipe @Type` and their dependencies are automatically resolved through the type system. The `cook` function validates the dependency graph and produces a `Managed` action.

### Database Connection Management

Database connections use a thread-local pattern:
1. A connection pool (`SqlitePool`) is created at startup
2. A `ThreadLocal Connection` stores the current connection for each thread
3. `Bornet.Sqlite.withConnection` allocates a connection from the pool and stores it in thread-local storage
4. Repository operations read the connection via `IO Connection`
5. The `hoistWithConnection` function decorates the `BornetServer` to ensure each request gets a connection

### Core Data Model

Defined in `Bornet.Model`:
- `Gero`: Caregiver entity with `GeroId` (newtype wrapper)
- `Turno`: Work shift with `TurnoId`
- `Dia`: Day of the schedule with `DiaId` and order
- `Jornada`: Join table linking a day, shift, and caregiver

All IDs use newtype wrappers for type safety.

### Repository Pattern

`Bornet.Repository` defines a record-of-functions "interface", basically like the repository pattern for data access.

The existing "implementations" provide values of this record type which interact with the sqlite database.

### Web API Structure

The API is defined using Servant's named routes (`NamedRoutes`) in `Bornet.Api`.

### Static Assets

Static files are served from `bornet/static/` via `Bornet.Api.WholeServer`, which combines the main API with a static file server under the `/static` route.

Configuration in `conf.yaml` specifies the `staticAssetsFolder`.

## Configuration

Application configuration is in `bornet/conf.yaml`.
