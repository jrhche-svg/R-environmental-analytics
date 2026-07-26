rm(list = ls())

# Packages
library(dplyr)
library(tidyr)
library(tibble)
library(networkD3)
library(htmlwidgets)
library(htmltools)

# -------------------------
# INPUT (example; replace with your data)
# -------------------------
df_wide <- tribble(
  ~Source, ~`Reach 1`, ~`Reach 2`, ~`Reach 3`, ~`Reach 4`, ~`Reach 5`,
  "Source A",   0,   20,  225,   206,   124,
  "Background", 424, 451, 664, 648, 568,
  "Treatment Plant", 0, 0, 213, 213, 213
)

# -------------------------
# TRANSFORM (measured loads at each reach by source)
# -------------------------
edges <- df_wide %>%
  pivot_longer(cols = -Source, names_to = "Target", values_to = "Value") %>%
  mutate(Value = as.numeric(Value)) %>%
  filter(!is.na(Value))

# Ordered labels
source_order <- c("Background", "Treatment Plant", "Source A")
target_order <- c("Reach 1", "Reach 2", "Reach 3", "Reach 4", "Reach 5")
river_name   <- "Example River"

# -------------------------
# NODES (sources + reaches + single river sink)
# -------------------------
nodes <- tibble(name = c(source_order, target_order, river_name)) %>%
  mutate(
    group = dplyr::case_when(
      name %in% source_order ~ name,   # nodes carry same group as sources for coloring
      name %in% target_order ~ "reach",
      name == river_name     ~ "river",
      TRUE ~ "other"
    )
  )

# -------------------------
# LINKS
# A) Source -> Reach links: measured loads (color by Source)
# B) Reach -> River links: each reach’s total load (grey)
# -------------------------
# A) Source -> Reach
links_sr <- edges %>%
  transmute(
    SourceName = Source,
    TargetName = Target,
    Value      = as.numeric(Value)
  ) %>%
  filter(!is.na(Value), Value > 0) %>%
  mutate(
    Source    = match(SourceName, nodes$name) - 1L,
    Target    = match(TargetName, nodes$name) - 1L,
    LinkGroup = SourceName
  ) %>%
  select(Source, Target, Value, LinkGroup)

# B) Reach -> River (sum across sources per reach)
reach_totals <- edges %>%
  group_by(Target) %>%
  summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop")

links_reach2river <- reach_totals %>%
  transmute(
    Source    = match(Target, nodes$name) - 1L,         # reach node
    Target    = match(river_name, nodes$name) - 1L,     # river sink
    Value     = as.numeric(Value),
    LinkGroup = "reach_to_river"
  ) %>%
  filter(Value > 0)

# Combine links
links <- bind_rows(links_sr, links_reach2river)

# -------------------------
# Per-link % of DOWNSTREAM target total (works for both layers)
# -------------------------
# Totals for each downstream node (all reaches + river)
river_total <- sum(reach_totals$Value, na.rm = TRUE)

target_totals_tbl <- bind_rows(
  reach_totals %>% rename(TargetNode = Target, TargetTotal = Value),
  tibble(TargetNode = river_name, TargetTotal = river_total)
)

# Map node names to 0-based indices used by networkD3
target_index_tbl <- tibble(
  TargetNode = c(target_order, river_name),
  TargetIdx  = match(c(target_order, river_name), nodes$name) - 1L
)

# Join totals to links by numeric Target index; compute per-link %
links <- links %>%
  left_join(target_index_tbl, by = c("Target" = "TargetIdx")) %>%
  left_join(target_totals_tbl, by = "TargetNode") %>%
  mutate(LinkPct = if_else(TargetTotal > 0, Value / TargetTotal, 0)) %>%
  select(Source, Target, Value, LinkGroup, LinkPct)

# -------------------------
# Colors (links by source; reach->river grey; river node black; reach nodes grey)
# -------------------------
colourScale <- JS(
  "d3.scaleOrdinal()
     .domain(['Source A','Background','Treatment Plant','reach_to_river','river','reach'])
     .range(['#1f77b4','#2ca02c','#d62728','#9e9e9e','#000000','#9e9e9e'])"
)

# -------------------------
# SANKEY
# -------------------------
sankey <- sankeyNetwork(
  Links       = links,
  Nodes       = nodes,
  Source      = "Source",
  Target      = "Target",
  Value       = "Value",
  NodeID      = "name",
  NodeGroup   = "group",
  LinkGroup   = "LinkGroup",
  fontSize    = 14,
  nodeWidth   = 28,
  nodePadding = 60,
  sinksRight  = TRUE,
  iterations  = 32,
  colourScale = colourScale
)

# -------------------------
# TOOLTIP: "<value> lbs/day (pp.pp%)", % = share of downstream node total
# -------------------------
sankey <- htmlwidgets::onRender(
  sankey,
  "
  function(el, x){
    var container = d3.select(el)
      .style('position','relative')
      .style('overflow','visible');
    var svg = container.select('svg');

    function fmtNum(n){ return Number(n||0).toLocaleString('en-US',{maximumFractionDigits:0}); }
    function fmtPct(p){ return (Number(p||0)*100).toFixed(1) + '%'; }

    function applyTitles(){
      var paths = svg.selectAll('path.link, path.sankey-link');
      if (paths.size()===0){ setTimeout(applyTitles, 60); return; }
      paths.each(function(d){
        var val = +((d && (d.value!=null?d.value:d.Value))||0);
        var pct = (d && (d.LinkPct!=null?d.LinkPct:d.linkPct)) || 0;
        d3.select(this).select('title').remove();
        d3.select(this).append('title')
          .text(fmtNum(val) + ' lbs/day (' + fmtPct(pct) + ')');
      });
    }

    var tip = container.select('div.sankey-tooltip');
    if (tip.empty()){
      tip = container.append('div').attr('class','sankey-tooltip')
        .style('position','absolute').style('pointer-events','none')
        .style('z-index','9999').style('background','rgba(255,255,255,0.96)')
        .style('border','1px solid #ddd').style('border-radius','10px')
        .style('padding','16px 20px').style('font-size','20px')
        .style('line-height','1.6').style('box-shadow','0 4px 20px rgba(0,0,0,0.12)')
        .style('display','none');
    }

    function showTooltipForPath(elem, ev){
      var d = d3.select(elem).datum(); if (!d) return;
      var val = +((d && (d.value!=null?d.value:d.Value))||0);
      var pct = (d && (d.LinkPct!=null?d.LinkPct:d.linkPct)) || 0;

      var rect = el.getBoundingClientRect();
      var left = (ev.clientX - rect.left) + 20;
      var top  = (ev.clientY - rect.top)  + 20;

      tip.html('<div><span style=\"font-weight:700;\">' + fmtNum(val) +
               '</span> lbs/day (' + fmtPct(pct) + ')</div>')
         .style('left', left + 'px').style('top', top + 'px').style('display','block');

      container.selectAll('path.link, path.sankey-link').style('stroke-opacity', 0.18);
      d3.select(elem).style('stroke-opacity', 0.9);
    }

    container.on('.globalhover', null);
    container.on('mousemove.globalhover', function(){
      var ev = d3.event || window.event; if (!ev) return;
      var elem = document.elementFromPoint(ev.clientX, ev.clientY);
      var p = elem, ok = false;
      while (p){
        if (p.tagName && p.tagName.toLowerCase()==='path'){
          var cls = (p.getAttribute('class')||'');
          if (/(^|\\s)(link|sankey-link)(\\s|$)/.test(cls)){ ok = true; break; }
        }
        p = p.parentNode; if (p === el) break;
      }
      if (ok){ showTooltipForPath(p, ev); }
      else {
        tip.style('display','none');
        container.selectAll('path.link, path.sankey-link').style('stroke-opacity', 0.35);
      }
    });
    container.on('mouseleave.globalhover', function(){
      tip.style('display','none');
      container.selectAll('path.link, path.sankey-link').style('stroke-opacity', 0.35);
    });

    function init(){ applyTitles(); setTimeout(applyTitles, 100); setTimeout(applyTitles, 300); }
    init();
    container.selectAll('.node').on('drag.__hov', init).on('dragend.__hov', init);
    if (window) d3.select(window).on('resize.__hov', init);
  }
  "
)

# -------------------------
# Title + Legends
# -------------------------
title_tag <- tags$h3(
  "Example River Daily Loading (mass/day) — Sources by Reach",
  style = "margin:0 0 8px 0; font-weight:600; font-family:Segoe UI, Roboto, Helvetica, Arial, sans-serif;"
)

legend_tag <- tags$div(
  style = "margin: 6px 0 12px 0; font-family:Segoe UI, Roboto, Helvetica, Arial, sans-serif; font-size: 12px;",
  tags$span(tags$b("Legend: ")),
  tags$span("Background = ambient contribution; "),
  tags$span("Source A = example source contribution; "),
  tags$span("Treatment Plant = example discharge contribution; "),
  tags$span("Reaches = Reach 1 through Reach 5; "),
  tags$span("River sink = Example River")
)

legend_item <- function(color, label) {
  tags$span(
    style = "display:inline-flex; align-items:center; margin-right:16px; margin-bottom:6px;",
    tags$span(style = paste0(
      "display:inline-block; width:16px; height:16px; border-radius:3px; ",
      "background:", color, "; margin-right:8px; border:1px solid rgba(0,0,0,0.15);"
    )),
    tags$span(label)
  )
}

bottom_legend <- tags$div(
  style = paste(
    "margin-top:12px; padding-top:10px; border-top:1px solid #eee;",
    "font-family:Segoe UI, Roboto, Helvetica, Arial, sans-serif; font-size:13px;"
  ),
  tags$div(style="font-weight:600; margin-bottom:8px;", "Color key"),
  legend_item("#2ca02c", "Background (source)"),
  legend_item("#d62728", "Treatment Plant (source)"),
  legend_item("#1f77b4", "Source A (source)"),
  legend_item("#9e9e9e", "Reach → River links"),
  legend_item("#000000", "Example River (sink node)")
)

# -------------------------
# % CONTRIBUTION TABLE (per reach + overall river)
# -------------------------
contrib_long <- edges %>%
  group_by(Target) %>%
  summarise(TargetTotal = sum(Value, na.rm = TRUE), .groups = "drop") %>%
  right_join(
    edges %>% group_by(Target, Source) %>%
      summarise(Flow = sum(Value, na.rm = TRUE), .groups = "drop"),
    by = "Target"
  ) %>%
  mutate(Pct = ifelse(TargetTotal > 0, Flow / TargetTotal, 0))

# Append overall river totals as an extra row
river_row <- contrib_long %>%
  group_by(Source) %>%
  summarise(Flow = sum(Flow, na.rm = TRUE), .groups = "drop") %>%
  mutate(Target = river_name, TargetTotal = sum(Flow), Pct = ifelse(TargetTotal > 0, Flow/TargetTotal, 0))

contrib_long <- bind_rows(contrib_long, river_row) %>%
  tidyr::complete(
    Target = c(target_order, river_name),
    Source = source_order,
    fill = list(Flow = 0, TargetTotal = 0, Pct = 0)
  ) %>%
  mutate(
    Target = factor(Target, levels = c(target_order, river_name)),
    Source = factor(Source, levels = source_order)
  ) %>%
  arrange(Target, Source)

pct_wide <- contrib_long %>%
  select(Target, Source, Pct) %>%
  tidyr::pivot_wider(names_from = Source, values_from = Pct) %>%
  mutate(across(all_of(source_order), ~ sprintf('%.1f%%', 100*.x)))

contrib_table_tag <- (function(df){
  header_cells <- c(list(tags$th("Target")), lapply(source_order, function(s) tags$th(s)))
  header <- do.call(tags$tr, header_cells)
  rows <- lapply(seq_len(nrow(df)), function(i){
    cells <- c(
      list(tags$td(as.character(df$Target[i]))),
      lapply(source_order, function(s) tags$td(df[[s]][i]))
    )
    do.call(tags$tr, cells)
  })
  tags$div(
    style = "margin-top:16px; font-family:Segoe UI, Roboto, Helvetica, Arial, sans-serif;",
    tags$div(
      "Source contributions by reach (plus overall river total)",
      style = "font-weight:600; margin-bottom:8px;"
    ),
    tags$table(
      style = paste(
        "border-collapse:collapse; width:100%; max-width:720px;",
        "font-size:13px; background:#fff; border:1px solid #e0e0e0;"
      ),
      tags$thead(header),
      tags$tbody(rows),
      tags$style(HTML("
        th, td { padding: 8px 10px; border-bottom: 1px solid #eee; text-align: right; }
        th:first-child, td:first-child { text-align: left; width: 200px; }
        thead th { border-bottom: 2px solid #ddd; font-weight:600; background:#fafafa; }
        tbody tr:hover { background: #fcfcfc; }
      "))
    )
  )
})(pct_wide)

# -------------------------
# Render
# -------------------------
browsable(tagList(title_tag, legend_tag, sankey, bottom_legend, contrib_table_tag))
