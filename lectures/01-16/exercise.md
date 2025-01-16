
```
<html>
    <head><title>Language Integrated Query (LINQ)</title></head>
    <body>
        <nav><ul>
          <li><a href="#overview">Query expression overview</a></li>
          <li><a href="#enable">How to enable LINQ querying of your data source</a></li>
          <li><a href="#providers">IQueryable LINQ providers</a></li>
        </ul></nav>
        <section id="overview">
          <ul>
            <li>Query expressions query and transform data from any LINQ-enabled data source.</li>
            <li>Query expressions use many familiar C# language constructs, which make them easy to read.</li>
          </ul>
        </section>
        <section id="enable">
          <h3>In-memory data</h3>
          <p>There are two ways you enable LINQ querying of in-memory data. If the data is of a type that implements <a href="https://learn.microsoft.com/en-us/dotnet/api/system.collections.generic.ienumerable-1">IEnumerable</a>, you query the data by using LINQ to Objects</p>
          <h3>Remote data</h3>
        </section>
        <section id="providers">
          <p>LINQ providers that implement IQueryable<T> can vary widely in their complexity.</p>
        </section>
    </body>
</html>
```

Design a DSL to make it easy to extract data from XML documents like the above. Below are some English descriptions of locations of data in the document. In the DSL we are designing it should be possible to precisely specify queries corresponding to each English description.

1. The contents of the second <h3> element in the section with id="enable".

```
;; ->
(list "Remote data")
```

2. The href attributes of all the links (<a> elements) in the nav

```
;; ->
(list "#overview" "#enable" "#providers")
```

3. The <li> elements that appear in sections (but not in the nav)

```
;; ->
(list '(li "Query expressions query and transform data from any LINQ-enabled data source.")
      '(li "Query expressions use many familiar C# language constructs, which make them easy to read."))
```

4. The ids of all sections that contain paragraph tags.

```
;; ->
(list "enable" "providers")
```

5. The paragraph (<p> element) appearing immediately after the <h3> header with text "In-memory data"

```
;; ->
(list
 '(p "There are two ways you enable LINQ querying of in-memory data. If the data is of a type that implements"
     (a (@ [href "https://learn.microsoft.com/en-us/dotnet/api/system.collections.generic.ienumerable-1"]) "IEnumerable")
     ", you query the data by using LINQ to Objects"))
```
