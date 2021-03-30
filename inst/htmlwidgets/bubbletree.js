HTMLWidgets.widget({

  name: 'bubbletree',

  type: 'output',
  factory: function(el, width, height) {
    var r = "a" + Math.random().toString(36).substring(7);
    // TODO: define shared variables for this instance

    return {

      renderValue: function(x) {


      function zoomed() {
          const currentTransform = d3.event.transform;
          d3.selectAll("#" + r + " g").attr("transform", currentTransform);
      }

      function dragstarted(d) {
          d3.event.sourceEvent.stopPropagation();
          d3.select(this).classed("dragging", true);
      }

      function dragged(d) {
          d3.select(this).attr("cx", d.x = d3.event.x).attr("cy", d.y = d3.event.y);
      }

      function dragended(d) {
          d3.select(this).classed("dragging", false);
      }

      let zoom = d3.zoom()
          .scaleExtent([1, 10])
          .on("zoom", zoomed);

      let drag = d3.drag()
          .subject(function (d) { return d; })
          .on("start", dragstarted)
          .on("drag", dragged)
          .on("end", dragended);



        const svg = d3.select("#" + el.id)
        .append("svg")
        .attr("id", r)
        //.attr("preserveAspectRatio", "XMaxYMin meet")
        //.attr("viewBox", [-width/2, -height/2, width, height]);
        doIt(width, height, x, r);
        svg.call(zoom);

      },

      resize: function(width, height) {

        resize(width, height, r);

      }

    };
  }
});
