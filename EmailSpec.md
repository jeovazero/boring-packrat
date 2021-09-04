From [RFC 5321](https://www.rfc-editor.org/rfc/pdfrfc/rfc5321.txt.pdf)

### Mailbox

```
Mailbox = Local-part "@" ( Domain / address-literal )
```

Limits:

    The maximum total length of a user name or other local-part is 64 octets.

    The maximum total length of a reverse-path or forward-path is 256 octets
    (including the punctuation and element separators).
    
    Forward-path = Path
    Path         = "<" [ A-d-l ":" ] Mailbox ">"
    
    The forward-path will contain at least a pair of angle brackets in
    addition to the Mailbox. This limits the Mailbox to 254 characters. 


#### Local-part

```
Local-part = Dot-string / Quoted-string
            ; MAY be case-sensitive

Dot-string = Atom *("." Atom)

Atom = 1*atext

Quoted-string = DQUOTE *QcontentSMTP DQUOTE

QcontentSMTP = qtextSMTP / quoted-pairSMTP

quoted-pairSMTP = %d92 %d32-126
                ; i.e., backslash followed by any ASCII
                ; graphic (including itself) or SPace

qtextSMTP = %d32-33 / %d35-91 / %d93-126
          ; i.e., within a quoted string, any
          ; ASCII graphic or space is permitted
          ; without blackslash-quoting except
          ; double-quote and the backslash itself.
```

#### Domain

```
Domain = sub-domain *("." sub-domain)

sub-domain = Let-dig [Ldh-str]

Let-dig = ALPHA / DIGIT

Ldh-str = *( ALPHA / DIGIT / "-" ) Let-dig
```

#### address-literal

```
address-literal = "[" ( IPv4-address-literal /
                  IPv6-address-literal /
                  General-address-literal ) "]"

IPv4-address-literal = Snum 3("." Snum)

IPv6-address-literal = "IPv6:" IPv6-addr

General-address-literal = Standardized-tag ":" 1*dcontent

Standardized-tag = Ldh-str
                  ; Standardized-tag MUST be specified in a
                  ; Standards-Track RFC and registered with IANA

dcontent = %d33-90 / ; Printable US-ASCII
            %d94-126  ; excl. "[", "\", "]"

Snum = 1*3DIGIT
     ; representing a decimal integer
     ; value in the range 0 through 255

IPv6-addr = IPv6-full / IPv6-comp / IPv6v4-full / IPv6v4-comp

IPv6-hex = 1*4HEXDIG

IPv6-full = IPv6-hex 7(":" IPv6-hex)

IPv6-comp = [IPv6-hex *5(":" IPv6-hex)] "::" [IPv6-hex *5(":" IPv6-hex)]
          ; The "::" represents at least 2 16-bit groups of
          ; zeros. No more than 6 groups in addition to the
          ; "::" may be present.

IPv6v4-full = IPv6-hex 5(":" IPv6-hex) ":" IPv4-address-literal

IPv6v4-comp = [IPv6-hex *3(":" IPv6-hex)] "::" [IPv6-hex *3(":" IPv6-hex) ":"] IPv4-address-literal
            ; The "::" represents at least 2 16-bit groups of
            ; zeros. No more than 4 groups in addition to the
            ; "::" and IPv4-address-literal may be present.
```

From [RFC 5322](https://datatracker.ietf.org/doc/html/rfc5322)

```
atext         = ALPHA / DIGIT /    ; Printable US-ASCII
                "!" / "#" /        ;  characters not including
                "$" / "%" /        ;  specials.  Used for atoms.
                "&" / "'" /
                "*" / "+" /
                "-" / "/" /
                "=" / "?" /
                "^" / "_" /
                "`" / "{" /
                "|" / "}" /
                "~"
```

From [RFC 5234](https://datatracker.ietf.org/doc/rfc5234/)

```

CR            = %x0D
              ; carriage return

CRLF          = CR LF
              ; Internet standard newline

CTL           = %x00-1F / %x7F
              ; controls

DIGIT         = %x30-39
              ; 0-9

DQUOTE        = %x22
              ; " (Double Quote)

HEXDIG        = DIGIT / "A" / "B" / "C" / "D" / "E" / "F"

HTAB          = %x09
              ; horizontal tab

LF            = %x0A
              ; linefeed

LWSP          = *(WSP / CRLF WSP)
              ; Use of this linear-white-space rule
              ;  permits lines containing only white
              ;  space that are no longer legal in
              ;  mail headers and have caused
              ;  interoperability problems in other
              ;  contexts.
              ; Do not use when defining mail
              ;  headers and use with caution in
              ;  other contexts.

OCTET         = %x00-FF
              ; 8 bits of data

SP            = %x20

VCHAR         = %x21-7E
              ; visible (printing) characters

WSP           = SP / HTAB
              ; white space
```
