# Prolog Interpreter in Haskell

A Prolog interpreter with limited syntax implemented in Haskell, designed to load Prolog databases from files and process user queries with support for multiple solutions.

## Features

### 🗃️ Database Loading
- Load Prolog databases from text files
- Supports facts and rules in a limited Prolog syntax

### 🔍 Query Processing
- Interactive command-line interface
- Finds all possible solutions for queries
- Supports basic backtracking

### ⚙️ Core Functionality
- Variable unification
- Rule application
- Multiple solution generation

## Project Structure

### Key Types
| Type          | Description                                  | Example                     |
|---------------|----------------------------------------------|-----------------------------|
| `Fact`        | Represents a Prolog fact                     | `parent(john, mary)`        |
| `Rule`        | Represents a rule with head and body         | `ancestor(X,Y):-parent(X,Y)`|
| `Atom`        | Predicate with arguments                     | `likes(anna, books)`        |
| `Substitution`| Variable bindings                            | `X = john`                  |
| `Context`     | Database state (facts + rules)               |                             |

### Main Components
1. **Parser** - Processes Prolog files into internal representation
2. **Unification Engine** - Handles variable binding and matching
3. **Query Solver** - Finds solutions for user queries

## Getting Started

### Prerequisites
- [Haskell Tool Stack](https://docs.haskellstack.org/)
- GHC 8.10 or newer

### Installation
```bash
git clone https://github.com/your-username/prolog-interpreter.git
cd prolog-interpreter
cabal build
cabal run
