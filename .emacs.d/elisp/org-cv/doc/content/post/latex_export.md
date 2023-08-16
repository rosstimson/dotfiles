+++
title = "Latex Exporter"
author = ["Óscar Nájera"]
draft = false
weight = 1004
+++

## Using modern-cv {#using-modern-cv}

[moderncv](https://www.ctan.org/tex-archive/macros/latex/contrib/moderncv) is a standard \\(\LaTeX\\) package that you can find in many of your
latex distributions. I maintain a fork of it, to work with my use case at
<https://github.com/Titan-C/moderncv.git> Feel free to use any or even your
personal fork for your desired use case.

To configure the export for moderncv you need the addition options in your
org file.

```org
# CV theme - options include: 'casual' (default), 'classic', 'oldstyle' and 'banking'
#+CVSTYLE: banking
# CV color - options include: 'blue' (default), 'orange', 'green', 'red', 'purple', 'grey' and 'black'
#+CVCOLOR: green
```

When exporting you can call the following function to get the latex file.

```emacs-lisp
(org-export-to-file 'moderncv "moderncv.tex")
(org-latex-compile "moderncv.tex")
```

<object data="moderncv.org.pdf" type="application/pdf" width="100%" height="500px">
<p>Alternative text - include a link <a href="moderncv.org.pdf">to the PDF!</a></p>
</object>


## Using alta-cv {#using-alta-cv}

[AltaCV](https://github.com/liantze/AltaCV) is another project to generate a CV, you will need to install it
yourself. I maintain a fork too at <https://github.com/Titan-C/AltaCV.git>
because I need extra features and I encourage to use this fork on the
`sections` branch.

The style of this CV is more involved and you need some configuration in
your org file to get it to work. First define the margins, the large margin
to the right is to allow for a second column.

```org
#+LATEX_HEADER: \geometry{left=1cm,right=9cm,marginparwidth=6.8cm,marginparsep=1.2cm,top=1.25cm,bottom=1.25cm}
```

Content on the right column has the same structure of a org file, but you
need to enclose it in the `\marginpar{}` command as shown next.

```org
#+latex: \marginpar{
```

```org
* Main Interests
- Free/Libre and Open Source Software (FLOSS)
- Free food
- Free beer

* Programming
- Python
- C/C++
- EmacsLisp
- Bash
- JavaScript
- PHP

* Languages

- *English*  Fluent
- *German*   Fluent
- *Spanish*  Native
- *French*   Intermediate
```

```org
#+latex: }
```

When exporting you can call the following function to get the latex file.

```emacs-lisp
(org-export-to-file 'altacv "altacv.tex")
(org-latex-compile "altacv.tex")
```

<object data="altacv.org.pdf" type="application/pdf" width="100%" height="500px">
<p>Alternative text - include a link <a href="altacv.org.pdf">to the PDF!</a></p>
</object>


## Using AwesomeCV {#using-awesomecv}

[AwesomeCV](https://github.com/posquit0/Awesome-CV) is another LaTeX template for producing nice-looking
CVs and cover letters. This style also supports some additional options. For example:

```org
# CV color - options include: 'awesome-red (default), 'awesome-emerald,
# 'awesome-skyblue', 'awesome-pink', 'awesome-orange', 'awesome-nephritis',
# 'awesome-concrete' and 'awesome-darknight', plus any standard color names.
#+CVCOLOR: awesome-red
# Specify the position and style of the photo
#+PHOTOSTYLE: right,noedge
```

When exporting you can call the following function to get the latex file.

```emacs-lisp
(org-export-to-file 'awesomecv "awesomecv.tex")
(org-latex-compile "awesomecv.tex")
```

Note that AwesomeCV uses the `fontspec` package, so you need to set `org-latex-compiler` to `lualatex` or `xelatex` for it to work.

In addition to the regular document attributes, the following are supported:

<div class="ox-hugo-table table table-striped">
<div></div>

| Field            | Description                                                 |
|------------------|-------------------------------------------------------------|
| PHOTOSTYLE       | Style of photo to use. Comma-separated values can include   |
|                  | circle/rectangle,edge/noedge,left/right.                    |
| CVCOLOR          | Color of highlights.                                        |
| STACKOVERFLOW    | Stack overflow info, must be specified as "`ID username`"   |
| FONTDIR          | Directory where the fonts can be found, defaults            |
|                  | to `fonts/` (as in the standard AwesomeCV)                  |
| CVHIGHLIGHTS     | Whether to colorize highlights. Defaults to true            |
| QUOTE            | Optional quote to include at the top of the CV              |
| FIRSTNAME        | First name to be shown in the CV. By default the first      |
|                  | space-separated part of AUTHOR is used.                     |
| LASTNAME         | Last name to be shown in the CV.  By default the second     |
|                  | space-separated part of AUTHOR is used.                     |
| CVFOOTER\_LEFT   | Text to include in the left footer. None by default         |
| CVFOOTER\_MIDDLE | Text to include in the middle footer. None by default.      |
| CVFOOTER\_RIGHT  | Text to include in the right footer. None by default.       |
| LATEX\_TITLE     | Text to use as the title section. \makecvheader by default. |
|                  | (Can specify \makecvheader[R] to justify to the right)      |

</div>


### CV environments {#cv-environments}

AwesomeCV supports a few additional types of environment types in `CV_ENV`,
including `cvemployer`, `cvskills`, `cvhonors` and `cvschool` (see full list below).
Some of these support additional property fields:

<div class="ox-hugo-table table table-striped">
<div></div>

| Field      | Description                                                          |
|------------|----------------------------------------------------------------------|
| FROM       | Start date of the entry                                              |
| TO         | End date of the entry                                                |
| DATE       | Shortcut to specify both `FROM` and `TO` as the same date.           |
|            | Both `FROM` and `TO` override `DATE`.                                |
| EMPLOYER   | Employer or organization, can also be specified                      |
|            | as `ORGANIZATION`, `SCHOOL`, `EVENT` or `POSITION` (different        |
|            | names make more sense depending on the type of environment)          |
| LABEL      | In `cvsubentry` environments, adds the given text to the left        |
|            | of the date range, can be used to add additional information         |
|            | to the entry.                                                        |
| RIGHT\_IMG | path to an image to include floating to the right of a `cventry`,    |
|            | a `cvsubentry` or `cvschool` entry. Meant to be used to show a logo. |
| PAGEBREAK  | Causes a LaTeX `\clearpage` statement to be inserted in the          |
|            | exported output before the heading.                                  |

</div>

All the supported values of `CV_ENV` for CVs are described below.


#### `cventries` {#cventries}

Enclose all the subheaders in a `cventries` environment. Subheaders can
be of type `cventry`, `cvschool`, or `cvemployer`.


#### `cvhonors` {#cvhonors}

Enclose all the subheaders in a `cvhonors` environment. Subheaders must
be of type `cvhonor`


#### `cventry` {#cventry}

Converts to a `\cventry` command. Supports attributes `FROM`, `TO`, `DATE`,
`EMPLOYER`, `LOCATION`, `RIGHT_IMG`.


#### `cvsubentry` {#cvsubentry}

Converts to a `\cvsubentry` command. Supports attributes `FROM`, `TO`, `DATE`,
`LABEL` `RIGHT_IMG`.


#### `cvemployer` {#cvemployer}

Converts to a `\cventry` with only the title line. Supports attributes
`FROM`, `TO`, `DATE` and `LOCATION`.


#### `cvschool` {#cvschool}

Converts to a `\cventry`. The headline should contain the degree
obtained, shown as the main title. Supports attributes `LOCATION`,
`SCHOOL`, `FROM`, `TO`, `DATE` and `RIGHT_IMG`.


#### `cvhonor` {#cvhonor}

Converts to a `\cvhonor` command (must be inside a `cvhonors`
headline). Supports attributes `LOCATION`, `EMPLOYER` (in this case `EVENT`
or `POSITION` might be more semantically accurate, and can also be
used), `FROM`, `TO`, `DATE`.


#### `cvskills` {#cvskills}

Converts to a `\cvskills` environment. The headline must contain a
[description list](https://orgmode.org/manual/Plain-lists.html), which gets converted into a sequence of `\cvskill`
commands, with the term as the skill title and the description as its
contents.

<object data="awesomecv.org.pdf" type="application/pdf" width="100%" height="500px">
<p>Alternative text - include a link <a href="awesomecv.org.pdf">to the PDF!</a></p>
</object>


### Cover letter environments {#cover-letter-environments}

AwesomeCV also supports generating cover letters. For this, `CV_ENV` can have a few additional values, shown below.


#### `letterheader` {#letterheader}

This environment provides heading/signature information for a cover letter. Supports attributes `RECIPIENT`. `EMPLOYER`, `LOCATION`, `LETTER_OPENING`, `LETTER_CLOSING`, `LETTER_ATTACHED`, `DATE`, `DATEFORMAT`.

Note that the text within the heading is not exported! You can use this, for example, to keep notes about your application or the employer. For example:

```org
* Recipient
:PROPERTIES:
:CV_ENV:   letterheader
:RECIPIENT: International Recruiting team
:EMPLOYER: Employer Co.
:LOCATION: Someplace, the world
:LETTER_OPENING: Dear International Recruiting team
:LETTER_CLOSING: Kind regards,
:LETTER_ATTACHED: Curriculum Vitae
:END:

Title and content are not exported.
Add any notes about the recipient here
They will *not* be exported.
```

<div class="ox-hugo-table table table-striped">
<div></div>

| Field            | Description                                                                      |
|------------------|----------------------------------------------------------------------------------|
| RECIPIENT        | Addressee E.g. Company Recruitment Team                                          |
| EMPLOYER         | Company name                                                                     |
| LOCATION         | Company address                                                                  |
| LETTER\_OPENING  | Letter opening, E.g. Dear Ms./Mr./Dr. LastName                                   |
| LETTER\_CLOSING  | Letter closing, E.g. Yours Sincerely,                                            |
| DATE             | The date used for the letter, uses \\\today as default if unspecified            |
| DATEFORMAT       | Specify an alternative date format for the letter header                         |
|                  | E.g. %e %M %Y might provide 19 March 2021                                        |
| LETTER\_ATTACHED | Attachments to the letter, will be listed at the bottom. E.g. "Curriculum Vitae" |

</div>


#### `cvletter` {#cvletter}

Converts to a `\cvletter` environment. This holds the content of a cover letter. The body can be subdivided using `lettersection` headings. The heading title is converted to a title line at the top of the letter.

```org
* Application for the position of /Awesome Job/ (job reference #123456)
:PROPERTIES:
:CV_ENV:   cvletter
:END:

** About Me
   :PROPERTIES:
   :CV_ENV:   lettersection
   :END:
Lorem ipsum dolor sit amet, consectetur adipiscing elit. Duis ullamcorper neque sit amet lectus facilisis sed luctus nisl iaculis. Vivamus at neque arcu, sed tempor quam. Curabitur pharetra tincidunt tincidunt. Morbi volutpat feugiat mauris, quis tempor neque vehicula volutpat. Duis tristique justo vel massa fermentum accumsan. Mauris ante elit, feugiat vestibulum tempor eget, eleifend ac ipsum. Donec scelerisque lobortis ipsum eu vestibulum. Pellentesque vel massa at felis accumsan rhoncus.

** Why Employer Co.?
   :PROPERTIES:
   :CV_ENV:   lettersection
   :END:
Suspendisse commodo, massa eu congue tincidunt, elit mauris pellentesque orci, cursus tempor odio nisl euismod augue. Aliquam adipiscing nibh ut odio sodales et pulvinar tortor laoreet. Mauris a accumsan ligula. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Suspendisse vulputate sem vehicula ipsum varius nec tempus dui dapibus. Phasellus et est urna, ut auctor erat. Sed tincidunt odio id odio aliquam mattis. Donec sapien nulla, feugiat eget adipiscing sit amet, lacinia ut dolor. Phasellus tincidunt, leo a fringilla consectetur, felis diam aliquam urna, vitae aliquet lectus orci nec velit. Vivamus dapibus varius blandit.

** Why me?
   :PROPERTIES:
   :CV_ENV:   lettersection
   :END:
 Duis sit amet magna ante, at sodales diam. Aenean consectetur porta risus et sagittis. Ut interdum, enim varius pellentesque tincidunt, magna libero sodales tortor, ut fermentum nunc metus a ante. Vivamus odio leo, tincidunt eu luctus ut, sollicitudin sit amet metus. Nunc sed orci lectus. Ut sodales magna sed velit volutpat sit amet pulvinar diam venenatis.
```


#### `cvletter_notitle` {#cvletter-notitle}

Same as `cvletter`, but does not include a letter title at the top.


#### `lettersection` {#lettersection}

Converts to a `\lettersection` command. These are the headline portions of a cover letter.

<object data="awesome-letter.org.pdf" type="application/pdf" width="100%" height="500px">
<p>Alternative text - include a link <a href="awesome-letter.org.pdf">to the PDF!</a></p>
</object>
