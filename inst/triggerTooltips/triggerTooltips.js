Shiny.addCustomMessageHandler("triggerTooltips", function (message) {
  const plotDiv = document.getElementById(message.plotID);
  const hoverPoints = message.tooltipPoints.map((point) => ({
    curveNumber: point.curve || 0,
    pointNumber: point.index,
  }));
  Plotly.Fx.hover(plotDiv, hoverPoints);
});
