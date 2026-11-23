# Citations in table notes

Some table notes cite the articles that describe a method (e.g., the
trim-and-fill method in
[`apa_publication_bias()`](https://wjschne.github.io/apa7/reference/apa_publication_bias.md)).
The references are in a BibTeX file that comes with apa7, and
`apa7_write_bib()` copies it to a document's folder so that the
citations in notes can be resolved and added to the reference list.

## Usage

``` r
apa7_write_bib(file = "apa7.bib", overwrite = TRUE)

apa7_bib()
```

## Arguments

- file:

  path of the BibTeX file to write

- overwrite:

  if `TRUE` (default), an existing file is replaced

## Value

`apa7_write_bib()` returns `file`, invisibly. `apa7_bib()` returns the
path of the BibTeX file that comes with apa7.

## Details

Citations in notes take one of two forms:

- text: "(Duval & Tweedie, 2000)", which the author must add to the
  reference list by hand.

- keys: Pandoc citation keys, "\[@duvalTrimFillSimple2000\]", which
  Pandoc's citeproc (and
  [apaquarto](https://wjschne.github.io/apaquarto/)) turns into
  formatted citations and reference-list entries, if the document's
  bibliography has the entries.

The form is set by the `citations` argument of
[`apa_note()`](https://wjschne.github.io/apa7/reference/apa_note.md), or
for all notes by `apa7_defaults(citations = )` (or
`options(apa7.citations = )`): `"auto"` (default), `"keys"`, or
`"text"`. With `"auto"`, a citation is written as a key when a Quarto or
R Markdown document is being rendered and its `bibliography` (in the
YAML header) has an entry with that key, and as text otherwise (e.g., in
the console, or in a table saved with
[`flextable::save_as_docx()`](https://davidgohel.github.io/flextable/reference/save_as_docx.html)).
So listing the file written by `apa7_write_bib()` in the YAML header is
enough:

    ---
    bibliography: [references.bib, apa7.bib]
    ---

with `apa7::apa7_write_bib()` in the setup chunk. The citation keys are
those that the Better BibTeX extension for Zotero makes by default
(e.g., `eggerBiasMetaanalysisDetected1997`), so entries exported from
Zotero with the same keys work, too.

Citations in notes from `apa_flextable(note = TRUE)` and the `apa-note`
chunk option of apaquarto are resolved when the document is rendered.
Citeproc does not see text that skips Pandoc (e.g., a flextable saved
directly to a Word file), so `"auto"` writes text there.

## Examples

``` r
# In a document's setup chunk: apa7_write_bib("apa7.bib")
path <- apa7_write_bib(tempfile(fileext = ".bib"))
readLines(path, n = 8)
#> [1] "% References cited in apa7 table notes."                                               
#> [2] "% Exported from the apa7 collection in Zotero (Better BibTeX citation keys)."          
#> [3] "% Write a copy to a document's folder with apa7::apa7_write_bib()."                    
#> [4] ""                                                                                      
#> [5] "@article{beggOperatingCharacteristicsRank1994,"                                        
#> [6] "  title = {Operating Characteristics of a Rank Correlation Test for Publication Bias},"
#> [7] "  author = {Begg, Colin B. and Mazumdar, Madhuchhanda},"                               
#> [8] "  year = 1994,"                                                                        
```
