triggerTooltips = function (message) {
  const plotElement = document.getElementById(message.plotID);
  const hoverPoints = message.tooltipPoints.map((point) => ({
    curveNumber: point.curve || 0,
    pointNumber: point.index,
  }));
  Plotly.Fx.hover(plotElement, hoverPoints);
};

Shiny.addCustomMessageHandler("triggerTooltips", triggerTooltips);

function triggerSelectedTooltips(plotID) {
  const plotElement = document.getElementById(plotID);
  const tooltipPoints = [];

  plotElement.data.forEach((trace, curveIndex) => {
    if (trace.selectedpoints && Array.isArray(trace.selectedpoints)) {
      trace.selectedpoints.forEach((pointIndex) => {
        tooltipPoints.push({
          x: trace.x[pointIndex],
          y: trace.y[pointIndex],
          curve: curveIndex,
          index: pointIndex,
        });
      });
    }
  });

  triggerTooltips({
    plotID: plotID,
    tooltipPoints: tooltipPoints,
  });
}
