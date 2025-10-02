Browser
    |
    V
Server
    |
    V
Database Query ->   Failure
    |                  |
    | (Success)        V
    |               API Interface
    |                  |
    |                  V
    |               Repeated Exec Commands + Parsing
    |                  |
    V                  V
Response (JSON) <- Repo Data Struct
    |
    V
Browser (Client)                   


