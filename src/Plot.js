/* global exports */

/*
  (C) 2017 David Lettier
  lettier.com
*/

"use strict";

var Chart = require('../../bower_components/chart.js');

exports.makePlot = (function () {
  var ctx = document.getElementById("pointChart");
  var plot = new Chart(
      ctx
    , {
        type: 'bar'
      , data: {
        datasets: [
          {
              label: ''
            , type: 'scatter'
            , data: []
            , pointBorderColor: '#87FFBF'
            , pointBackgroundColor: '#87FFBF'
            , pointRadius: 7
            , showLine: false
          },
          {
              label: ''
            , type: 'line'
            , data: []
            , fill: false
            , borderColor: '#DC84F4'
            , borderWidth: 5
            , pointRadius: 0
            , showLine: true
          }
        ]
      }
      , options: {
          responsive: false
        , legend: {
            display: false
          }
        , scales: {
          xAxes: [
            {
                type: 'linear'
              , position: 'bottom'
              , gridLines: {
                    color: '#aaa'
                  , zeroLineColor: '#aaa'
                }
              , ticks: {
                    fontColor: '#aaa'
                }
            }
          ]
          , yAxes: [
            {
                gridLines: {
                    color: '#aaa'
                  , zeroLineColor: '#aaa'
                }
              , ticks: {
                    fontColor: '#aaa'
                }
            }
          ]
        }
      }
    }
  );
  return function (data) {
    plot.data.datasets[0].data = data.scatter || [];
    plot.data.datasets[1].data = data.line || [];
    plot.update();
  };
}());
