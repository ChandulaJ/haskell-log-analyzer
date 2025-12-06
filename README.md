# Haskell Server Log Analyzer

A functional programming project for analyzing web server access logs.

## Features

- Parse Apache/Nginx Combined Log Format
- Statistical analysis of HTTP requests
- Traffic pattern analysis
- Error detection and reporting
- Bot detection and classification

## Usage

```bash
stack build
stack exec server-log-analyzer-exe data/server.log
```

## Data Format

The analyzer processes logs in Apache Combined Log Format:
```
IP - - [timestamp] "METHOD path HTTP/version" status size "referer" "user-agent"
```
