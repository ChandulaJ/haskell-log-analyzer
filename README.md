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


## Parser Module Overview With Data Types
The Parser module processes Apache/Nginx Combined Log Format entries and converts them into structured Haskell data types for analysis.

## Input Format

The parser accepts log lines in Apache Combined Log Format:

```
IP - - [timestamp] "METHOD path HTTP/version" status size "referer" "user-agent" "other"
```

### Example Input:
```
54.36.149.41 - - [22/Jan/2019:03:56:14 +0330] "GET /filter/test HTTP/1.1" 200 30577 "-" "Mozilla/5.0 (compatible; AhrefsBot/6.1)" "-"
```

### Input Components:
- **IP Address**: Client IP (e.g., `54.36.149.41`)
- **Timestamp**: Date and time with timezone (e.g., `22/Jan/2019:03:56:14 +0330`)
- **HTTP Method**: GET, POST, PUT, DELETE, etc.
- **Path**: Requested URL path (supports URL encoding)
- **HTTP Version**: Protocol version (e.g., `1.1`)
- **Status Code**: HTTP response code (e.g., `200`, `404`, `500`)
- **Response Size**: Bytes transferred (e.g., `30577`)
- **Referrer**: Source URL or `"-"` if none
- **User Agent**: Browser/bot identification string

## Output Format

### Single Line Parsing: `parseLogLine`
**Input:** `String` (single log line)  
**Output:** `Maybe LogEntry`

Returns `Just LogEntry` if parsing succeeds, `Nothing` if it fails.

### Multiple Lines Parsing: `parseLogFile`
**Input:** `String` (entire log file content)  
**Output:** `[LogEntry]` (list of successfully parsed entries)

Failed lines are silently skipped.

## Output Data Structure

```haskell
data LogEntry = LogEntry
  { leIpAddress :: String          -- "54.36.149.41"
  , leTimestamp :: UTCTime          -- 2019-01-22 00:26:14 UTC
  , leMethod :: HttpMethod          -- GET
  , lePath :: String                -- "/filter/test"
  , leHttpVersion :: String         -- "1.1"
  , leStatusCode :: Int             -- 200
  , leResponseSize :: Int           -- 30577
  , leReferrer :: String            -- "" or URL
  , leUserAgent :: String           -- "Mozilla/5.0..."
  }

data HttpMethod = GET | POST | PUT | DELETE | HEAD | OPTIONS | PATCH | UNKNOWN String
```

## Additional Functions

### `parseMethod :: String -> Maybe HttpMethod`
Converts string to HttpMethod enum.

**Input:** `"GET"`  
**Output:** `Just GET`

### `parseTimestamp :: String -> Maybe UTCTime`
Parses Apache timestamp format to UTCTime.

**Input:** `"22/Jan/2019:03:56:14 +0330"`  
**Output:** `Just 2019-01-22 00:26:14 UTC`

### `classifyBot :: String -> BotType`
Identifies bot type from user agent string.

**Input:** `"Mozilla/5.0 (compatible; Googlebot/2.1)"`  
**Output:** `GoogleBot`

**Bot Types:** `GoogleBot`, `BingBot`, `AhrefsBot`, `OtherBot`, `Browser`
