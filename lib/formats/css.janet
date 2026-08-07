(def value
  ```
  The default stylesheet for HTML pages
  ```
```
body {
  font-family: sans-serif;
  font-size: 16px;
  margin: 0 auto;
  max-width: 1024px;
  width: 90%;
}

div.manpage {
  margin: 2rem 0;
}

div.manpage div.header, div.manpage div.footer {
  display: flex;
}

div.manpage div.header > div, div.manpage div.footer > div {
  flex: 0 0 20%;
}

div.manpage div.header div.man-sec, div.manpage div.footer > div.date {
  flex: 1;
  text-align: center;
}

div.manpage div.header > div:last-child, div.manpage div.footer > div:last-child {
  text-align: right;
}

div.manpage div.footer {
  border-top: 1px dotted black;
  margin-top: 2rem;
  padding-top: 0.5rem;
}

div.manpage h2.section {
  border-bottom: 0.15rem dotted;
}

div.manpage h3.subsection {
  margin-bottom: 0.75rem;
}

div.manpage p.synopsis {
  display: grid;
  column-gap: 0.5em;
  grid-template-columns: auto 1fr;
}

div.manpage p.synopsis span.name {
  grid-column: 1;
}

div.manpage p.synopsis span.rest {
  grid-column: 2;
}

div.manpage em {
  font-style: normal;
  text-decoration: underline;
}

div.manpage span {
  word-break: keep-all;
}

div.manpage span.command, div.manpage span.name {
  font-weight: 600;
}

div.manpage span.arg-opt, div.manpage span.arg-mod {
  font-weight: 600;
}

div.manpage span.arg-param {
  text-decoration: underline;
}

div.manpage table {
  margin-top: 1.0rem;
}

div.manpage ul {
  padding-left: 1.0rem;
}

div.manpage ul.tagged-list, div.manpage ul.indented-list {
  list-style: none;
  padding-left: 0;
}

div.manpage ul.tagged-list li, div.manpage ul.indented-list li {
  margin-top: 0.25rem;
  margin-left: 2.0rem;
}

div.manpage ul.tagged-list h4 {
  margin-bottom: 0;
  margin-left: -1.0rem;
}

div.manpage ul.indented-list h4 {
  margin-bottom: 0;
}

div.manpage ul.tagged-list h4 + p, div.manpage ul.indented-list h4 + p {
  margin-top: 0;
}

div.manpage .compact {
  margin: 1em 0;
}

div.manpage .compact > li > p {
  margin: 0;
}

div.manpage div.codeblock {
  margin-left: 2rem;
  overflow-x: auto;
  white-space: pre;
}

div.manpage pre {
  margin-left: 2rem;
  overflow-x: auto;
}

div.manpage blockquote {
  border-left: 5px solid #d3d3d3;
  color: #808080;
  margin-left: 2rem;
  padding-left: 1rem;
}

@media (max-width: 600px) {
  div.manpage blockquote {
    margin-left: 0;
  }
}
```)
