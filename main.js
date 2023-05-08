//Charlie Morris
//I create a bar chart, a scatterplot, and a line graph to represent the Dartmouth Class Date

//Great graph 1
$(document).ready(function(){
    // Load the data from a JSON file
    $.getJSON("barChart.json", function(data){
        // Extract the categories and values from the data
        var categories = [];
        var values = [];
        var enrollments = [];
        $.each(data, function(index, row){
            categories.push(row.Department);
            values.push(row['Average GPA']);
            enrollments.push(row.Enrollments);
        });

        //Assign colors based on enrollments
        var colors = [];
        $.each(enrollments, function(index, enrollment){
            if (enrollment < 1000){
                colors.push('blue');
            }
            else if (enrollment >= 1000 && enrollment < 1500){
                colors.push('green');
            }
            else {
                colors.push('red');
            }
        });

        //Actually make the Bar Chart
        //Include title and axes
        var ctx = $("#myChart1");
        var myChart = new Chart(ctx, {
            type: 'bar',
            data: {
                labels: categories,
                datasets: [{
                    data: values,
                    backgroundColor: colors
                }]
            },
            options: {
                layout: {
                  padding: {
                    left: 50,
                    right: 50,
                    top: 20,                        
                    bottom: 20
                    }
                },
                scales: {
                  x: {
                    title: {
                      display: true,                          
                      text: 'Department'
                    }
                  },
                  y: {
                    title: {
                      display: true,
                      text: 'Average GPA Points from a Class'
                    }
                  }
                },
                  plugins: {
                    title: {
                        display: true,
                        text: 'Bar Chart Exposing Grades by Department (Blue: 500-1000 students, Green 1000-1500, Red 1500+)',
                        fontSize: 18
                    }
                }
            }
        });
    });
});






//Create Graph 2
$(document).ready(function() {
    createChart();
  });
  
  function createChart() {
    // Load the data from a JSON file
    $.getJSON("scatterplot.json", function(data) {
      // Extract the categories and values from the data
      var categories = [];
      var values = [];
      var colors = [];
  
      $.each(data, function(index, row) {
        categories.push(row.Department);
        values.push({x: row['Course Number'], y: row['Median GPA Points']});
  
        //Assign colors based on department
        if (row.Department == 'PHYS') {
          colors.push('blue');
        } else if (row.Department == 'ECON') {
          colors.push('green');
        } else if (row.Department == 'CHEM') {
          colors.push('red');
        } else if (row.Department == 'BIOL') {
          colors.push('yellow');
        } else if (row.Department == 'GOVT') {
          colors.push('black');
        }
      });
  
      //Actually make the Scatterplot
      //Include title and axes
      var ctx = document.getElementById("myChart2").getContext("2d");
  
      var myChart = new Chart(ctx, {
        type: 'scatter',
        data: {
          datasets: [{
            label: '(Course Number, GPA)',
            data: values,
            backgroundColor: colors
          }]
        },
        options: {
            layout: {
                padding: {
                  left: 50,
                  right: 50,
                  top: 20,                        
                  bottom: 20
                  }
              },
              scales: {
                x: {
                  title: {
                    display: true,                          
                    text: 'Course Number'
                  }
                },
                y: {
                  title: {
                    display: true,
                    text: 'Median GPA Points'
                  }
                }
              },
            plugins: {
                title: {
                    display: true,
                    text: 'Scatter Plot Exploring how Course Number Relates to Grades (Blue: PHYS, Green: ECON, Red: CHEM, Yellow: BIOL, Black: GOVT)',
                    fontSize: 18,
                    lineHeight: 1.2
                }
            }
        }
      });
    });
  }
  


// Third Graph
// Load the data from JSON file
$.getJSON('lineGraph.json', function(data) {
  
    // Create dictionary to store the color for each department-course pair
    var colors = {'GOVT40.09': 'red', 'PSYC1': 'green', 'SOCY48': 'blue', 'ENGS24': 'purple'};
    
    // Store datasets
    var datasets = [];
    
    // Group by each individual class
    var groups = _.groupBy(data, function(d) { return d.Department + d['Course Number']; });
    
    // Iterate through each group
    _.forEach(groups, function(groupData, groupName) {
      
      // Grab info for each group
      var groupColor = colors[groupName];
      var groupMedianGPA = _.map(groupData, 'Median GPA Points');
      var groupTime = _.map(groupData, 'Time');
  
      var dataset = {
        label: groupName,
        borderColor: groupColor,
        backgroundColor: groupColor,
        fill: false,
        data: []
      };
  
      // Construct the data array and store all the line seguments
      for (var i = 0; i < groupData.length; i++) {
        var dataPoint = {
          x: groupTime[i],
          y: groupMedianGPA[i]
        };
        dataset.data.push(dataPoint);
        if (i > 0 && groupData[i].CourseNumber == groupData[i-1].CourseNumber) {
          // Draw line segment between this data point and the previous data point in the same course
          var previousDataPoint = {
            x: groupTime[i-1],
            y: groupMedianGPA[i-1]
          };
          var lineSegment = {
            x1: previousDataPoint.x,
            y1: previousDataPoint.y,
            x2: dataPoint.x,
            y2: dataPoint.y
          };
          dataset.data.push(lineSegment);
        }
      }
      datasets.push(dataset);
    });
    
    //Actually make the Line Graph
    //Include title and axes
    var ctx = document.getElementById('myChart3').getContext('2d');
    var scatterChart = new Chart(ctx, {
        type: 'line',
        data: {
            datasets: datasets,
        },
        options: {
            layout: {
              padding: {
                left: 50,
                right: 50,
                top: 20,                        
                bottom: 20
                }
            },
            scales: {
              x: {
                type: 'linear',
                position: 'bottom',
                title: {
                  display: true,                          
                  text: 'Term'
                },
                ticks: {
                    stepSize: 1,
                    min: 1,
                    max: 3
                  }
              },
              y: {
                title: {
                  display: true,
                  text: 'Average GPA in the Class'
                }
              }
            },
            plugins: {
                title: {
                    display: true,
                    text: 'Median GPA Changing over Time (1: Fall 2021, 2: Winter 2022, 3: Spring 2022)',
                    fontSize: 18,
                    lineHeight: 1.2
                }
            }
        }
    });
});