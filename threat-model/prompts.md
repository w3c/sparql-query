# Prompts

Prompts used to produce the SPARQL 1.2 Query Language threat model, transcribed
from the working session in order. Spelling and obvious slips have been
corrected; the wording is otherwise as given.

Italic entries in square brackets are not prompts — they are local session
commands or mid-turn interjections, included so the sequence is complete.

---

*[`/model` Fable 5]*  
*[`/effort max`]*

**1.**
> Write a threat model for SPARQL query in document `spec/index.html`. Write the report out in markdown.

*[`/model` — Opus 5]*  
*[`/effort xhigh`]*

**2.**
> Split the threat model into 3 separate documents, written to directory "`dev/`".
>
> The first covers sections 1,2,3,4,B and is called "`threat-model`".
>
> The second is section "spec recommendations", and is called "`threat-model-spec-actions`"
>
> The third is the remaining sections and is called "`threat-model-advice`".
>
> Write in both markdown and HTML. Style the HTML the same as before.

**3.**
> The overall goal is that eventually, the first part will be in the spec and the other two parts will be as related work documents. So the spec part is analysis and description, and not what to do about it. Now renumber sections in all documents and revise content so that they are three separate documents. In particular, "`threat-model`" should be standalone. The other two parts can refer and build on "`threat-model`". "`threat-model`" should not link to the other two parts. Add a short overview document "`threat-model-overview`" that has a list of the other documents and their roles. Update documents in `dev/`. There is a commit of `snapshots/` where the different versions have been put for safety.


**4.**
> In `dev/threat-model.html`: (1) include one level of subsections in the table of contents. (2) Change section 3 to use numbering, not letters. (3) Change scope near the top of the document to remove the word "only"

**5.**
> mirror the renumbering and the wording change to `threat-model.md`

**6.**
> Produce "`threat-model/sparql-threat-model.html`", starting with `dev/threat-model.html` and style it like `spec/index.html` including using W3C respec.
