# Decorate Module Output

## Introduction

The outputs produced by `teal` modules, like graphs or tables, are
created by the module developer and look a certain way. It is hard to
design an output that will satisfy every possible user, so the form of
the output should be considered a default value that can be customized.
Here we describe the concept of *decoration*, enabling the app developer
to tailor outputs to their specific requirements without rewriting the
original module code.

The decoration process is build upon transformation procedures,
introduced in `teal`. While `transformators` are meant to edit module’s
input, decorators are meant to adjust the module’s output. To
distinguish the difference, modules in `teal.modules.general` have 2
separate parameters: `transformators` and `decorators`.

To get a complete understanding refer the following vignettes:

- Transforming the input data in [this
  vignette](https://insightsengineering.github.io/teal/latest-tag/articles/transform-input-data.html).
- Transforming module output in [this
  vignette](https://insightsengineering.github.io/teal/latest-tag/articles/transform-module-output.html).

## Outputs that can be decorated

It is important to note which output objects from a given module can be
decorated. The module function documentation’s *Decorating Module*
section has this information.

You can also refer the table shown below to know which module outputs
can be decorated.

| Module | Output (Class) |
|----|----|
| `tm_a_pca` | elbow_plot (ggplot), circle_plot (ggplot), biplot (ggplot), eigenvector_plot (ggplot) |
| `tm_a_regression` | plot (ggplot) |
| `tm_g_association` | plot (grob) |
| `tm_g_bivariate` | plot (ggplot) |
| `tm_g_distribution` | histogram_plot (ggplot), qq_plot (ggplot), summary_table (datatables), test_table (datatables) |
| `tm_g_response` | plot (ggplot) |
| `tm_g_scatterplot` | plot (ggplot) |
| `tm_g_scatterplotmatrix` | plot (patchwork/ggplot) |
| `tm_missing_data` | summary_plot (grob), combination_plot (grob), by_subject_plot (ggplot), table (datatables) |
| `tm_outliers` | box_plot (ggplot), density_plot (ggplot), cumulative_plot (ggplot), table (datatables) |
| `tm_t_crosstable` | table (ElementaryTable) |

Also, note that there are five different types of objects that can be
decorated:

1.  `ElementaryTable`
2.  `ggplot`
3.  `patchwork`
4.  `grob`
5.  `datatables`

*Tip:* A general tip before trying to decorate the output from the
module is to copy the reproducible code and running them in a separate R
session to quickly iterate the decoration you want.

## Decorating `ElementaryTable`

Here’s an example to showcase how you can edit an output of class
`ElementaryTable`. `rtables` modifiers like
[`rtables::insert_rrow`](https://insightsengineering.github.io/rtables/latest-tag/reference/insert_rrow.html)
can be applied to modify this object.

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`teal.modules.general`](https://insightsengineering.github.io/teal.modules.general/)`)`\
\
`data`` ``<-`` ``teal_data``(``join_keys ``=`` ``default_cdisc_join_keys``[`[`c`](https://rdrr.io/r/base/c.html)`(``"ADSL"``, ``"ADRS"``)``]``)`\
`data`` ``<-`` `[`within`](https://rdrr.io/r/base/with.html)`(``data``, ``{`\
`  `[`require`](https://rdrr.io/r/base/library.html)`(`[`nestcolor`](https://insightsengineering.github.io/nestcolor/)`)`\
`  ``ADSL`` ``<-`` ``rADSL`\
`}``)`\
\
`insert_rrow_decorator`` ``<-`` ``function``(``default_caption`` ``=`` ``"I am a good new row"``)`` ``{`\
`  ``teal_transform_module``(`\
`    label ``=`` ``"New row"``,`\
`    ui ``=`` ``function``(``id``)`` ``{`\
`      ``shiny``::`[`textInput`](https://rdrr.io/pkg/shiny/man/textInput.html)`(``shiny``::`[`NS`](https://rdrr.io/pkg/shiny/man/NS.html)`(``id``, ``"new_row"``)``, ``"New row"``, value ``=`` ``default_caption``)`\
`    ``}``,`\
`    server ``=`` ``function``(``id``, ``data``)`` ``{`\
`      ``moduleServer``(``id``, ``function``(``input``, ``output``, ``session``)`` ``{`\
`        ``reactive``(``{`\
`          `[`data`](https://rdrr.io/r/utils/data.html)`(``)`` ``|>`\
`            `[`within`](https://rdrr.io/r/base/with.html)`(`\
`              ``{`\
`                ``table`` ``<-`` ``rtables``::`[`insert_rrow`](https://insightsengineering.github.io/rtables/latest-tag/reference/insert_rrow.html)`(``table``, ``rtables``::`[`rrow`](https://insightsengineering.github.io/rtables/latest-tag/reference/rrow.html)`(``new_row``)``)`\
`              ``}``,`\
`              new_row ``=`` ``input``$``new_row`\
`            ``)`\
`        ``}``)`\
`      ``}``)`\
`    ``}`\
`  ``)`\
`}`\
\
`app`` ``<-`` ``init``(`\
`  data ``=`` ``data``,`\
`  modules ``=`` ``modules``(`\
`    `[`tm_t_crosstable`](https://insightsengineering.github.io/teal.modules.general/reference/tm_t_crosstable.md)`(`\
`      label ``=`` ``"Cross Table"``,`\
`      x ``=`` ``data_extract_spec``(`\
`        dataname ``=`` ``"ADSL"``,`\
`        select ``=`` ``select_spec``(`\
`          choices ``=`` ``variable_choices``(``data``[[``"ADSL"``]``]``, subset ``=`` ``function``(``data``)`` ``{`\
`            ``idx`` ``<-`` ``!`[`vapply`](https://rdrr.io/r/base/lapply.html)`(``data``, ``inherits``, `[`logical`](https://rdrr.io/r/base/logical.html)`(``1``)``, `[`c`](https://rdrr.io/r/base/c.html)`(``"Date"``, ``"POSIXct"``, ``"POSIXlt"``)``)`\
`            `[`names`](https://rdrr.io/r/base/names.html)`(``data``)``[``idx``]`\
`          ``}``)``,`\
`          selected ``=`` ``"COUNTRY"``,`\
`          multiple ``=`` ``TRUE``,`\
`          ordered ``=`` ``TRUE`\
`        ``)`\
`      ``)``,`\
`      y ``=`` ``data_extract_spec``(`\
`        dataname ``=`` ``"ADSL"``,`\
`        select ``=`` ``select_spec``(`\
`          choices ``=`` ``variable_choices``(``data``[[``"ADSL"``]``]``, subset ``=`` ``function``(``data``)`` ``{`\
`            ``idx`` ``<-`` `[`vapply`](https://rdrr.io/r/base/lapply.html)`(``data``, ``is.factor``, `[`logical`](https://rdrr.io/r/base/logical.html)`(``1``)``)`\
`            `[`names`](https://rdrr.io/r/base/names.html)`(``data``)``[``idx``]`\
`          ``}``)``,`\
`          selected ``=`` ``"SEX"`\
`        ``)`\
`      ``)``,`\
`      decorators ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(`\
`        table ``=`` ``insert_rrow_decorator``(``)`\
`      ``)`\
`    ``)`\
`  ``)`\
`)`\
\
`if`` ``(`[`interactive`](https://rdrr.io/r/base/interactive.html)`(``)``)`` ``{`\
`  ``shinyApp``(``app``$``ui``, ``app``$``server``)`\
`}`

## Decorating `ggplot`

Here’s an example to showcase how you can edit an output of class
`ggplot`. You can extend them using `ggplot2` functions.

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`teal.modules.general`](https://insightsengineering.github.io/teal.modules.general/)`)`\
\
`data`` ``<-`` ``teal_data``(``join_keys ``=`` ``default_cdisc_join_keys``[`[`c`](https://rdrr.io/r/base/c.html)`(``"ADSL"``, ``"ADRS"``)``]``)`\
`data`` ``<-`` `[`within`](https://rdrr.io/r/base/with.html)`(``data``, ``{`\
`  `[`require`](https://rdrr.io/r/base/library.html)`(`[`nestcolor`](https://insightsengineering.github.io/nestcolor/)`)`\
`  ``ADSL`` ``<-`` ``rADSL`\
`}``)`\
\
`ggplot_caption_decorator`` ``<-`` ``function``(``default_caption`` ``=`` ``"I am a good decorator"``)`` ``{`\
`  ``teal_transform_module``(`\
`    label ``=`` ``"Caption"``,`\
`    ui ``=`` ``function``(``id``)`` ``{`\
`      ``shiny``::`[`textInput`](https://rdrr.io/pkg/shiny/man/textInput.html)`(``shiny``::`[`NS`](https://rdrr.io/pkg/shiny/man/NS.html)`(``id``, ``"footnote"``)``, ``"Footnote"``, value ``=`` ``default_caption``)`\
`    ``}``,`\
`    server ``=`` ``function``(``id``, ``data``)`` ``{`\
`      ``moduleServer``(``id``, ``function``(``input``, ``output``, ``session``)`` ``{`\
`        ``reactive``(``{`\
`          `[`data`](https://rdrr.io/r/utils/data.html)`(``)`` ``|>`\
`            `[`within`](https://rdrr.io/r/base/with.html)`(`\
`              ``{`\
`                ``plot`` ``<-`` ``plot`` ``+`` ``ggplot2``::`[`labs`](https://ggplot2.tidyverse.org/reference/labs.html)`(``caption ``=`` ``footnote``)`\
`              ``}``,`\
`              footnote ``=`` ``input``$``footnote`\
`            ``)`\
`        ``}``)`\
`      ``}``)`\
`    ``}`\
`  ``)`\
`}`\
\
`app`` ``<-`` ``init``(`\
`  data ``=`` ``data``,`\
`  modules ``=`` ``modules``(`\
`    `[`tm_a_regression`](https://insightsengineering.github.io/teal.modules.general/reference/tm_a_regression.md)`(`\
`      label ``=`` ``"Regression"``,`\
`      response ``=`` ``data_extract_spec``(`\
`        dataname ``=`` ``"ADSL"``,`\
`        select ``=`` ``select_spec``(`\
`          label ``=`` ``"Select variable:"``,`\
`          choices ``=`` ``"BMRKR1"``,`\
`          selected ``=`` ``"BMRKR1"``,`\
`          multiple ``=`` ``FALSE``,`\
`          fixed ``=`` ``TRUE`\
`        ``)`\
`      ``)``,`\
`      regressor ``=`` ``data_extract_spec``(`\
`        dataname ``=`` ``"ADSL"``,`\
`        select ``=`` ``select_spec``(`\
`          label ``=`` ``"Select variables:"``,`\
`          choices ``=`` ``variable_choices``(``data``[[``"ADSL"``]``]``, `[`c`](https://rdrr.io/r/base/c.html)`(``"AGE"``, ``"SEX"``, ``"RACE"``)``)``,`\
`          selected ``=`` ``"AGE"``,`\
`          multiple ``=`` ``TRUE``,`\
`          fixed ``=`` ``FALSE`\
`        ``)`\
`      ``)``,`\
`      decorators ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(`\
`        plot ``=`` ``ggplot_caption_decorator``(``"I am a Regression"``)`\
`      ``)`\
`    ``)`\
`  ``)`\
`)`\
\
`if`` ``(`[`interactive`](https://rdrr.io/r/base/interactive.html)`(``)``)`` ``{`\
`  ``shinyApp``(``app``$``ui``, ``app``$``server``)`\
`}`

## Decorating `grob`

Here’s an example to showcase how you can edit an output of class
`grob`. You can extend them using `grid` and `gridExtra` functions.

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`teal.modules.general`](https://insightsengineering.github.io/teal.modules.general/)`)`\
\
`data`` ``<-`` ``teal_data``(``join_keys ``=`` ``default_cdisc_join_keys``[`[`c`](https://rdrr.io/r/base/c.html)`(``"ADSL"``, ``"ADRS"``)``]``)`\
`data`` ``<-`` `[`within`](https://rdrr.io/r/base/with.html)`(``data``, ``{`\
`  ``ADSL`` ``<-`` ``rADSL`\
`}``)`\
\
`grob_caption_decorator`` ``<-`` ``function``(``default_caption`` ``=`` ``"I am a good decorator"``)`` ``{`\
`  ``teal_transform_module``(`\
`    label ``=`` ``"Caption"``,`\
`    ui ``=`` ``function``(``id``)`` ``{`\
`      ``shiny``::`[`textInput`](https://rdrr.io/pkg/shiny/man/textInput.html)`(``shiny``::`[`NS`](https://rdrr.io/pkg/shiny/man/NS.html)`(``id``, ``"footnote"``)``, ``"Footnote"``, value ``=`` ``default_caption``)`\
`    ``}``,`\
`    server ``=`` ``function``(``id``, ``data``)`` ``{`\
`      ``moduleServer``(``id``, ``function``(``input``, ``output``, ``session``)`` ``{`\
`        ``reactive``(``{`\
`          `[`data`](https://rdrr.io/r/utils/data.html)`(``)`` ``|>`\
`            `[`within`](https://rdrr.io/r/base/with.html)`(`\
`              ``{`\
`                ``footnote_grob`` ``<-`` ``grid``::`[`textGrob`](https://rdrr.io/r/grid/grid.text.html)`(`\
`                  ``footnote``,`\
`                  x ``=`` ``0``, hjust ``=`` ``0``,`\
`                  gp ``=`` ``grid``::`[`gpar`](https://rdrr.io/r/grid/gpar.html)`(``fontsize ``=`` ``10``, fontface ``=`` ``"italic"``, col ``=`` ``"gray50"``)`\
`                ``)`\
`                ``plot`` ``<-`` ``gridExtra``::`[`arrangeGrob`](https://rdrr.io/pkg/gridExtra/man/arrangeGrob.html)`(`\
`                  ``plot``,`\
`                  ``footnote_grob``,`\
`                  ncol ``=`` ``1``,`\
`                  heights ``=`` ``grid``::`[`unit.c`](https://rdrr.io/r/grid/unit.c.html)`(`\
`                    ``grid``::`[`unit`](https://rdrr.io/r/grid/unit.html)`(``1``, ``"npc"``)`` ``-`` ``grid``::`[`unit`](https://rdrr.io/r/grid/unit.html)`(``1``, ``"lines"``)``, ``grid``::`[`unit`](https://rdrr.io/r/grid/unit.html)`(``1``, ``"lines"``)`\
`                  ``)`\
`                ``)`\
`              ``}``,`\
`              footnote ``=`` ``input``$``footnote`\
`            ``)`\
`        ``}``)`\
`      ``}``)`\
`    ``}`\
`  ``)`\
`}`\
\
`app`` ``<-`` ``init``(`\
`  data ``=`` ``data``,`\
`  modules ``=`` ``modules``(`\
`    `[`tm_g_association`](https://insightsengineering.github.io/teal.modules.general/reference/tm_g_association.md)`(`\
`      ref ``=`` ``data_extract_spec``(`\
`        dataname ``=`` ``"ADSL"``,`\
`        select ``=`` ``select_spec``(`\
`          choices ``=`` ``variable_choices``(`\
`            ``data``[[``"ADSL"``]``]``,`\
`            `[`c`](https://rdrr.io/r/base/c.html)`(``"SEX"``, ``"RACE"``, ``"COUNTRY"``, ``"ARM"``, ``"STRATA1"``, ``"STRATA2"``, ``"ITTFL"``, ``"BMRKR2"``)`\
`          ``)``,`\
`          selected ``=`` ``"RACE"`\
`        ``)`\
`      ``)``,`\
`      vars ``=`` ``data_extract_spec``(`\
`        dataname ``=`` ``"ADSL"``,`\
`        select ``=`` ``select_spec``(`\
`          choices ``=`` ``variable_choices``(`\
`            ``data``[[``"ADSL"``]``]``,`\
`            `[`c`](https://rdrr.io/r/base/c.html)`(``"SEX"``, ``"RACE"``, ``"COUNTRY"``, ``"ARM"``, ``"STRATA1"``, ``"STRATA2"``, ``"ITTFL"``, ``"BMRKR2"``)`\
`          ``)``,`\
`          selected ``=`` ``"BMRKR2"``,`\
`          multiple ``=`` ``TRUE`\
`        ``)`\
`      ``)``,`\
`      decorators ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(`\
`        plot ``=`` ``grob_caption_decorator``(``"I am a Association"``)`\
`      ``)`\
`    ``)`\
`  ``)`\
`)`\
\
`if`` ``(`[`interactive`](https://rdrr.io/r/base/interactive.html)`(``)``)`` ``{`\
`  ``shinyApp``(``app``$``ui``, ``app``$``server``)`\
`}`

## Decorating `datatables`

Here’s an example to showcase how you can edit an output of class
`datatables`. Please refer the [helper
functions](https://rstudio.github.io/DT/functions.html) of the `DT`
package to learn more about extending the `datatables` objects.

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`teal.modules.general`](https://insightsengineering.github.io/teal.modules.general/)`)`\
\
`data`` ``<-`` ``teal_data``(``join_keys ``=`` ``default_cdisc_join_keys``[`[`c`](https://rdrr.io/r/base/c.html)`(``"ADSL"``, ``"ADRS"``)``]``)`\
`data`` ``<-`` `[`within`](https://rdrr.io/r/base/with.html)`(``data``, ``{`\
`  `[`require`](https://rdrr.io/r/base/library.html)`(`[`nestcolor`](https://insightsengineering.github.io/nestcolor/)`)`\
`  ``ADSL`` ``<-`` ``rADSL`\
`}``)`\
`fact_vars_adsl`` ``<-`` `[`names`](https://rdrr.io/r/base/names.html)`(`[`Filter`](https://rdrr.io/r/base/funprog.html)`(``isTRUE``, `[`sapply`](https://rdrr.io/r/base/lapply.html)`(``data``[[``"ADSL"``]``]``, ``is.factor``)``)``)`\
`vars`` ``<-`` ``choices_selected``(``variable_choices``(``data``[[``"ADSL"``]``]``, ``fact_vars_adsl``)``)`\
\
`dt_table_decorator`` ``<-`` ``function``(``color1`` ``=`` ``"pink"``, ``color2`` ``=`` ``"lightblue"``)`` ``{`\
`  ``teal_transform_module``(`\
`    label ``=`` ``"Table color"``,`\
`    ui ``=`` ``function``(``id``)`` ``{`\
`      ``selectInput``(`\
`        ``NS``(``id``, ``"color"``)``,`\
`        ``"Table Color"``,`\
`        choices ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"white"``, ``color1``, ``color2``)``,`\
`        selected ``=`` ``"Default"`\
`      ``)`\
`    ``}``,`\
`    server ``=`` ``function``(``id``, ``data``)`` ``{`\
`      ``moduleServer``(``id``, ``function``(``input``, ``output``, ``session``)`` ``{`\
`        ``reactive``(``{`\
`          `[`data`](https://rdrr.io/r/utils/data.html)`(``)`` ``|>`` `[`within`](https://rdrr.io/r/base/with.html)`(`\
`            ``{`\
`              ``summary_table`` ``<-`` ``DT``::`[`formatStyle`](https://rdrr.io/pkg/DT/man/formatCurrency.html)`(`\
`                ``summary_table``,`\
`                columns ``=`` `[`attr`](https://rdrr.io/r/base/attr.html)`(``summary_table``$``x``, ``"colnames"``)``[``-``1``]``,`\
`                target ``=`` ``"row"``,`\
`                backgroundColor ``=`` ``color`\
`              ``)`\
`            ``}``,`\
`            color ``=`` ``input``$``color`\
`          ``)`\
`        ``}``)`\
`      ``}``)`\
`    ``}`\
`  ``)`\
`}`\
\
`app`` ``<-`` ``init``(`\
`  data ``=`` ``data``,`\
`  modules ``=`` ``modules``(`\
`    `[`tm_g_distribution`](https://insightsengineering.github.io/teal.modules.general/reference/tm_g_distribution.md)`(`\
`      dist_var ``=`` ``data_extract_spec``(`\
`        dataname ``=`` ``"ADSL"``,`\
`        select ``=`` ``select_spec``(`\
`          choices ``=`` ``variable_choices``(``data``[[``"ADSL"``]``]``, `[`c`](https://rdrr.io/r/base/c.html)`(``"AGE"``, ``"BMRKR1"``)``)``,`\
`          selected ``=`` ``"BMRKR1"``,`\
`          multiple ``=`` ``FALSE``,`\
`          fixed ``=`` ``FALSE`\
`        ``)`\
`      ``)``,`\
`      strata_var ``=`` ``data_extract_spec``(`\
`        dataname ``=`` ``"ADSL"``,`\
`        filter ``=`` ``filter_spec``(`\
`          vars ``=`` ``vars``,`\
`          multiple ``=`` ``TRUE`\
`        ``)`\
`      ``)``,`\
`      group_var ``=`` ``data_extract_spec``(`\
`        dataname ``=`` ``"ADSL"``,`\
`        filter ``=`` ``filter_spec``(`\
`          vars ``=`` ``vars``,`\
`          multiple ``=`` ``TRUE`\
`        ``)`\
`      ``)``,`\
`      decorators ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(`\
`        summary_table ``=`` ``dt_table_decorator``(``)`\
`      ``)`\
`    ``)`\
`  ``)`\
`)`

    ## Initializing tm_g_distribution

\
`if`` ``(`[`interactive`](https://rdrr.io/r/base/interactive.html)`(``)``)`` ``{`\
`  ``shinyApp``(``app``$``ui``, ``app``$``server``)`\
`}`

## Decorating `patchwork`

Here’s an example to showcase how you can edit an output of class
`patchwork`. Since `patchwork` objects support ggplot2-style addition,
you can use
[`patchwork::plot_annotation()`](https://patchwork.data-imaginist.com/reference/plot_annotation.html)
and standard ggplot2 theme modifications.

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`teal.modules.general`](https://insightsengineering.github.io/teal.modules.general/)`)`\
\
`data`` ``<-`` ``teal_data``(``join_keys ``=`` ``default_cdisc_join_keys``[`[`c`](https://rdrr.io/r/base/c.html)`(``"ADSL"``, ``"ADRS"``)``]``)`\
`data`` ``<-`` `[`within`](https://rdrr.io/r/base/with.html)`(``data``, ``{`\
`  `[`require`](https://rdrr.io/r/base/library.html)`(`[`nestcolor`](https://insightsengineering.github.io/nestcolor/)`)`\
`  ``ADSL`` ``<-`` ``rADSL`\
`  ``ADRS`` ``<-`` ``rADRS`\
`}``)`\
\
`patchwork_title_decorator`` ``<-`` ``function``(``default_title`` ``=`` ``"I am a good decorator"``)`` ``{`\
`  ``teal_transform_module``(`\
`    label ``=`` ``"Title"``,`\
`    ui ``=`` ``function``(``id``)`` ``shiny``::`[`textInput`](https://rdrr.io/pkg/shiny/man/textInput.html)`(``shiny``::`[`NS`](https://rdrr.io/pkg/shiny/man/NS.html)`(``id``, ``"title"``)``, ``"Plot Title"``, value ``=`` ``default_title``)``,`\
`    server ``=`` ``function``(``id``, ``data``)`` ``{`\
`      ``moduleServer``(``id``, ``function``(``input``, ``output``, ``session``)`` ``{`\
`        ``reactive``(``{`\
`          `[`data`](https://rdrr.io/r/utils/data.html)`(``)`` ``|>`\
`            `[`within`](https://rdrr.io/r/base/with.html)`(`\
`              ``{`\
`                ``plot`` ``<-`` ``plot`` ``+`` ``patchwork``::`[`plot_annotation`](https://patchwork.data-imaginist.com/reference/plot_annotation.html)`(``title ``=`` ``plot_title``)`\
`              ``}``,`\
`              plot_title ``=`` ``input``$``title`\
`            ``)`\
`        ``}``)`\
`      ``}``)`\
`    ``}`\
`  ``)`\
`}`\
\
`app`` ``<-`` ``init``(`\
`  data ``=`` ``data``,`\
`  modules ``=`` ``modules``(`\
`    `[`tm_g_scatterplotmatrix`](https://insightsengineering.github.io/teal.modules.general/reference/tm_g_scatterplotmatrix.md)`(`\
`      label ``=`` ``"Scatterplot matrix"``,`\
`      variables ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(`\
`        ``data_extract_spec``(`\
`          dataname ``=`` ``"ADSL"``,`\
`          select ``=`` ``select_spec``(`\
`            choices ``=`` ``variable_choices``(``data``[[``"ADSL"``]``]``)``,`\
`            selected ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"AGE"``, ``"RACE"``, ``"SEX"``)``,`\
`            multiple ``=`` ``TRUE``,`\
`            ordered ``=`` ``TRUE`\
`          ``)`\
`        ``)``,`\
`        ``data_extract_spec``(`\
`          dataname ``=`` ``"ADRS"``,`\
`          filter ``=`` ``filter_spec``(`\
`            label ``=`` ``"Select endpoints:"``,`\
`            vars ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"PARAMCD"``, ``"AVISIT"``)``,`\
`            choices ``=`` ``value_choices``(``data``[[``"ADRS"``]``]``, `[`c`](https://rdrr.io/r/base/c.html)`(``"PARAMCD"``, ``"AVISIT"``)``, `[`c`](https://rdrr.io/r/base/c.html)`(``"PARAM"``, ``"AVISIT"``)``)``,`\
`            selected ``=`` ``"INVET - END OF INDUCTION"``,`\
`            multiple ``=`` ``TRUE`\
`          ``)``,`\
`          select ``=`` ``select_spec``(`\
`            choices ``=`` ``variable_choices``(``data``[[``"ADRS"``]``]``)``,`\
`            selected ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"AGE"``, ``"AVAL"``, ``"ADY"``)``,`\
`            multiple ``=`` ``TRUE``,`\
`            ordered ``=`` ``TRUE`\
`          ``)`\
`        ``)`\
`      ``)``,`\
`      decorators ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(`\
`        plot ``=`` ``patchwork_title_decorator``(``"I am a Scatterplot matrix"``)`\
`      ``)`\
`    ``)`\
`  ``)`\
`)`\
\
`if`` ``(`[`interactive`](https://rdrr.io/r/base/interactive.html)`(``)``)`` ``{`\
`  ``shinyApp``(``app``$``ui``, ``app``$``server``)`\
`}`
