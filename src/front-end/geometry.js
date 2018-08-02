var CANVAS_WIDTH    = 800
var CANVAS_HEIGHT   = 700
var DEBUG           = true
// var DEBUG           = false



//////////////////// MAIN
////////////////////////////////////////

function main() {
    // console.log("started penrose geometry test module")
    var s = Snap("#svgdiv");
    $("#svgdiv").css("width",  CANVAS_WIDTH);
    $("#svgdiv").css("height", CANVAS_HEIGHT);

    //testIntersect(s)
    // var l1 = [ 10, 10, 100, 100 ]
    // var l2 = [ 50, 150, 250, 50 ]
    var l1 = genLine([0, CANVAS_WIDTH], [0, CANVAS_HEIGHT])
    var l2 = genLine([0, CANVAS_WIDTH], [0, CANVAS_HEIGHT])
    // var l2 = JSON.parse(JSON.stringify(l1))
    // l2.pt1.y += 100
    // l2.pt2.y += 100
    // renderPolylines(s, [toList([l1.pt1, l1.pt2]), toList([l2.pt1, l2.pt2])])
    // //console.log(l1pts)
    //shortestAbsDistance(l1, l2, s)


    // randmly create two polygons
    var poly1 = randomPolygon(300);
    var poly2 = randomPolygon(110);
    // render to screen
    drawPolygon(poly1, s)
    drawPolygon(poly2, s)
    // check which points are contained in the other polygon
    checkAllPointContainment(poly1, poly2, s)
    checkAllPointContainment(poly2, poly1, s)

    // find and draw abs min, min, and max distances
    absMinDist(poly1, poly2, s)
    minDist(poly1, poly2, s)
    maxDist(poly1, poly2, s)
}

//////////////////// POLYGON DISTANCE FUNCTIONS
////////////////////////////////////////

/****************************************
* passed in p1, p2, returns nothing
* draws absolute minimum distance between two polygons
*****************************************/
function absMinDist(p1, p2, s){
  if(DEBUG) console.log("...Calculating abs min distance (in blue)")
  absMin = {}
  absMin.dist = 100000
  for(var i = 0; i < p1.length-1; i++){
    for(var j = 0; j < p2.length-1; j++){
      var a = {}
      var b = {}
      a.pt1 = p1[i]
      a.pt2 = p1[i+1]
      b.pt1 = p2[j]
      b.pt2 = p2[j+1]

      temp = shortestAbsDistance(a, b, p1, p2, s)  // for every point against every edge
                                                  // find shortest distance, NOT DIRECTIONAL
                                                  // NO SIGN
      if (temp.dist < absMin.dist){
        absMin.dist = temp.dist
        absMin.line = temp.line
      }
    }
  }

  // draw absolute minimum line or point of intersection
  if(DEBUG) console.log("Abs min distance is " + absMin.dist)
  if(absMin.dist != 0){
    var l = s.polyline(absMin.line.pt1.x, absMin.line.pt1.y, absMin.line.pt2.x, absMin.line.pt2.y)
    l.attr({
      stroke: "blue",
  		strokeWidth: 2
    })
  }
  else{
    var c = s.circle(absMin.line.pt1.x, absMin.line.pt1.y, 5);
    c.attr({
      fill: "blue"
    })
  }
}

/****************************************
* most positive/maximum distance between polygons
*****************************************/
function maxDist(p1, p2, s){
      if(DEBUG) console.log("...Calculating max distance (in yellow)")
      max = {}
      max.dist = Number.MIN_SAFE_INTEGER
      for(var i = 0; i < p1.length-1; i++){
        for(var j = 0; j < p2.length-1; j++){
          var a = {}
          var b = {}
          a.pt1 = p1[i]
          a.pt2 = p1[i+1]
          b.pt1 = p2[j]
          b.pt2 = p2[j+1]

          temp = shortestDirectionalDistance(a, b, p1, p2, s) // for every point
                                                    // against every edge find
                                                    // shortest distance, DIRECTIONAL
                                                    // SIGNED
          if (temp.dist > max.dist){
            max.dist = temp.dist
            max.line = temp.line
          }
        }
      }

      // draw maximum line
      if(DEBUG) console.log("Max distance is " + max.dist)
      if(max.dist != 0){
        var l = s.polyline(max.line.pt1.x, max.line.pt1.y, max.line.pt2.x, max.line.pt2.y)
        l.attr({
          stroke: "yellow",
      		strokeWidth: 1
        })
      }
      else{
        var c = s.circle(max.line.pt1.x, max.line.pt1.y, 5);
        c.attr({
          fill: "yellow"
        })
      }
  }


/****************************************
* most negative/minimum distance between polygons
*****************************************/
function minDist(p1, p2, s){
    if(DEBUG) console.log("...Calculating min distance (in purple)")
    min = {}
    min.dist = Number.MAX_SAFE_INTEGER
    for(var i = 0; i < p1.length-1; i++){
      for(var j = 0; j < p2.length-1; j++){
        var a = {}
        var b = {}
        a.pt1 = p1[i]
        a.pt2 = p1[i+1]
        b.pt1 = p2[j]
        b.pt2 = p2[j+1]

        temp = shortestDirectionalDistance(a, b, p1, p2, s)  // for every point
                                                  // against every edge find
                                                  // shortest distance, DIRECTIONAL
                                                  // SIGNED
        if (temp.dist < min.dist){
          min.dist = temp.dist
          min.line = temp.line
        }
      }
    }

      // draw minimum line
    if(DEBUG) console.log("Min distance is " + min.dist)
    if(min.dist != 0){
      var l = s.polyline(min.line.pt1.x, min.line.pt1.y, min.line.pt2.x, min.line.pt2.y)
      l.attr({
        stroke: "purple",
    		strokeWidth: 1
      })
    }
    else{
      var c = s.circle(min.line.pt1.x, min.line.pt1.y, 5);
      c.attr({
        fill: "purple"
      })
    }
}

/****************************************
* Finds shortest absolute distance, (accounts for interseciton means dist == 0)
*****************************************/
function shortestAbsDistance(l1, l2, poly1, poly2, s){
    var ret = {}
    dists = []
    var inter = intersect(toList([l1.pt1, l1.pt2]), toList([l2.pt1, l2.pt2]))
    if(inter){ //T
      var l1 = { pt1: { x:inter[0], y:inter[1] } }
      ret = {line: l1, dist: 0}
      return ret
    }
    else{
      dists[0] = distToSegment(l1.pt1, l2, s)
      dists[1] = distToSegment(l1.pt2, l2, s)
      dists[2] = distToSegment(l2.pt1, l1, s)
      dists[3] = distToSegment(l2.pt2, l1, s)
      var minimum = Number.MAX_SAFE_INTEGER
      for(var i = 0; i < 4; i++){
          if(dists[i].dist < minimum){
              minimum = dists[i].dist
              ret = dists[i]
          }
        }
    }
    var line = [ret.line.pt1.x, ret.line.pt1.y, ret.line.pt2.x, ret.line.pt2.y]
    // console.log("SHORTEST DISTANCE " + dists[index].dist);
    return ret
}


/****************************************
* Finds shortest signed distance, does not account for intersection/boundary distance
*****************************************/
function shortestDirectionalDistance(l1, l2, poly1, poly2, s){
    dists = []

    dists[0] = distToSegment(l1.pt1, l2, s)
    var sign = inPolygon(l1.pt1, poly2)
    if(sign){ dists[0].dist *= (-1) }

    dists[1] = distToSegment(l1.pt2, l2, s)
    var sign = inPolygon(l1.pt2, poly2)
    if(sign){ dists[1].dist *= (-1)}

    dists[2] = distToSegment(l2.pt1, l1, s)
    //var apt = 
    var sign = inPolygon(l2.pt1, poly1) // get sign from intersection point on A
    if(sign){ dists[2].dist *= (-1)}

    dists[3] = distToSegment(l2.pt2, l1, s)
    var sign = inPolygon(l2.pt2, poly1) // get sign from intersection point on A
    if(sign){ dists[3].dist *= (-1)}

    var minimum = Number.MAX_SAFE_INTEGER
    var index = -1
    for(var i = 0; i < 4; i++){
        if(Math.abs(dists[i].dist) < minimum){
            minimum = Math.abs(dists[i].dist)
            index = i
        }
    }
  //  if(DEBUG) console.log("SHORTEST DISTANCE " + dists[index].dist);
    return dists[index]
}
/****************************************
* finds shortest dist of a single point to a line segment
* returns object with line in coord form and distance
*****************************************/
function distToSegment(pt, l, s) {
    [v, w] = [l.pt1, l.pt2]
    var l2 = dist2(v, w);
    if (l2 == 0) return dist2(pt, v);
    var t = ((pt.x - v.x) * (w.x - v.x) + (pt.y - v.y) * (w.y - v.y)) / l2;
    t = Math.max(0, Math.min(1, t));
    var pt1 = { x: pt.x, y: pt.y }
    var pt2 = { x: v.x + t * (w.x - v.x), y: v.y + t * (w.y - v.y) }
    var line = { pt1, pt2 }
    dist = Math.sqrt(dist2(pt, { x: v.x + t * (w.x - v.x),
        y: v.y + t * (w.y - v.y) }))
    return {line, dist}
}

/****************************************
* Based on https://github.com/substack/point-in-polygon/
    // ray-casting algorithm based on
    // http://www.ecse.rpi.edu/Homepages/wrf/Research/Short_Notes/pnpoly.html

    Returns true if point is contained in polygon vs
*****************************************/
function inPolygon(point, vs) {
    var x = point.x;
    var y = point.y;
    var inside = false;
    for (var i = 0, j = vs.length - 1; i < vs.length; j = i++) {
        var xi = vs[i].x, yi = vs[i].y;
        var xj = vs[j].x, yj = vs[j].y;

        var intersect = ((yi > y) != (yj > y))
            && (x < (xj - xi) * (y - yi) / (yj - yi) + xi);
        if (intersect) inside = !inside;
    }
    return inside;
};


/****************************************
* Helper distance funcitons from
* https://stackoverflow.com/questions/849211/shortest-distance-between-a-point-and-a-line-segment
*****************************************/
function sqr(x) {
  return x * x
}
function dist2(v, w) {
  return sqr(v.x - w.x) + sqr(v.y - w.y)
}

/*****************************************
* line intercept math by Paul Bourke http://paulbourke.net/geometry/pointlineplane/
* Determine the intersection point of two line segments
* Return FALSE if the lines don't intersect
*****************************************/
function intersect([x1, y1, x2, y2], [x3, y3, x4, y4]) {
  // Check if none of the lines are of length 0
	if ((x1 === x2 && y1 === y2) || (x3 === x4 && y3 === y4)) {
		return false
	}
	denominator = ((y4 - y3) * (x2 - x1) - (x4 - x3) * (y2 - y1))
  // Lines are parallel
	if (denominator === 0) {
        // lines are colinear
        numeratorA  = ((x4 - x3) * (y1 - y3) - (y4 - y3) * (x1 - x3))
        numeratorB  = ((x2 - x1) * (y1 - y3) - (y2 - y1) * (x1 - x3))
        if(numeratorA == 0 && numeratorB == 0)
            return true
		return false
	}
	let ua = ((x4 - x3) * (y1 - y3) - (y4 - y3) * (x1 - x3)) / denominator
	let ub = ((x2 - x1) * (y1 - y3) - (y2 - y1) * (x1 - x3)) / denominator
  // is the ion along the segments
	if (ua < 0 || ua > 1 || ub < 0 || ub > 1) {
		return false
	}
  // Return a object with the x and y coordinates of the intersection
	let x = x1 + ua * (x2 - x1)
	let y = y1 + ua * (y2 - y1)
	return [x, y]
}


/****************************************
* Given coordinates, make a flat list
*****************************************/
function toList(coords) {
    lst = []
    coords.forEach(function(pt) {
      lst.push(pt.x)
      lst.push(pt.y)
    });
    return lst
}

/****************************************
* Given an array of Coordinate objects, draw a polygon
*****************************************/
function drawPolygon(polygon, s){
  pts = []
  count = 0
  for(var i = 0; i < polygon.length; i++){
    pts[count] = polygon[i].x
    pts[count+1] = polygon[i].y
    count += 2
  }
  var randColor = Math.floor(Math.random() * 999)
  var colorStr = "#" + randColor

  var p = s.polyline(pts)
  p.attr({
    fill: colorStr,
    "fill-opacity": 0.5,
		stroke: "black",
		strokeWidth: 2
  })
  p.drag()
}


/*************************************
* Checks whether points in p1 are inside p2 (if visualizepoints
* flag, then shows green for contained points, red for noncontained)
**************************************/
function checkAllPointContainment(p1, p2, s){
  var inside = false;
  var visualizePoints = true

  for(var i = 0; i < p1.length; i++){
    if (inPolygon(p1[i], p2)){
      inside = true;
      fillColor = "#0C0"
    } else fillColor = "#F00"
    if (visualizePoints){
      var point = s.circle(p1[i].x, p1[i].y, 4)
      point.attr({
        fill: fillColor
      })
    }
  }
  return inside;
}

/*************************************
* Creates a random polygon with random num vertices, center, concave/convex
**************************************/
function randomPolygon(rmaxnew){
  var xCenter = Math.floor(Math.random() * 300) + 200;
  var yCenter = Math.floor(Math.random() * 300) + 200;
  var rmax = rmaxnew, rmin =40;
  var pointmax = 16, pointmin = 3;
  var numPoints = Math.floor(Math.random() * (pointmax-pointmin)) + pointmin;
  if(DEBUG){
    console.log("New Polygon: \n numPoints is " + numPoints + "\n Center: " + xCenter + " " + yCenter)
  }
  poly = []
  for(var i = 0; i < numPoints; i++){
    var pt = {}
    var r = Math.floor(Math.random() * (rmax-rmin)) + rmin;
    // console.log("R at " + i + " is " + r);
    pt.x = (xCenter + r * Math.cos(2 * Math.PI * i / numPoints))
    pt.y = (yCenter + r * Math.sin(2 * Math.PI * i / numPoints));
    poly.push(pt)
  }
  var end = {}
  end.x = poly[0].x
  end.y = poly[0].y
  poly.push(end)

  return poly
}

/*************************************
* Draws a polygon from an SVG by taking all points along the hull of a polygon
**************************************/
function drawHullPointsFromSVG(inputSVG, s){
  newpoints = []
  count = 0;
  parsedSVG = parsePathString(inputSVG)
  for (var i in parsedSVG) {
  	if (parsedSVG[i].code === 'Q' || parsedSVG[i].code === 'M' || parsedSVG[i].code === 'C'	|| parsedSVG[i].code === 'L' || parsedSVG[i].code === 'T'){
  		newpoints[count] = parsedSVG[i].x
  		newpoints[count+1] = parsedSVG[i].y
  		count+=2
  	}
  	else if (parsedSVG[i].code === 'H'){
  		newpoints[count] = parsedSVG[i].x
  		newpoints[count+1] = newpoints[count-1]
  		count+=2
  	}
  	else if (parsedSVG[i].code === 'V'){
  		newpoints[count] = newpoints[count-1]
  		newpoints[count+1] = parsedSVG[i].y
  		count+=2
  	}
  	else if (parsedSVG[i].code === 'Z'){ //TWO NEGATIVE numbers means there is a break in the polgon
  		newpoints[count] = newpoints[0]    //similar to "pick up pen, move, put down pen"
  		newpoints[count+1] = newpoints[1]
  		newpoints[count+2] = -1           //negative numbers are placeholders for "pen up"
  		newpoints[count+3] = -1
  		count+=4
  	}
  	//TODO add in A elliptical
  	else
  		count = count
  }
  return newpoints
}

/////////////////////////////////////////////////////////////////
////////////////// MISC HELPER FUNCTIONS AND POLY LINE FUNCTIONS
/////////////////////////////////////////////////////////////////



////// POLY Lines
//////////////////////////////////////////

/****************************************
* Render intersection point
*****************************************/
function renderIntersect(s, l1, l2) {
    renderPolylines(s, [l1, l2])
    p = intersect(l1, l2)
    if(p) {
        if (p === true) {
            console.log("The lines are colinear!")
        } else {
            console.log("Intersection found at: ", p)
            renderPoint(s, p)
        }
    }
    else {
        console.log("The lines do not intersect!")
    }
}
/****************************************
* Render lines in pt form
*****************************************/
function renderPolylines(s, ptss) {
    var res = []
    ptss.forEach(function(pts) {
        res += renderPolyline(s, pts)
    })
    return res
}

/****************************************
* renders a poly line where line is in pt1, pt2 coord form
*****************************************/
function renderPolyline(s, line) {
    if(DEBUG) console.log("rendering", pts);
    var x1 = line.pt1.x, y1 = line.pt1.y
    var x2 = line.pt2.x, y1 = line.pt2.y
    var renderedLine = s.polyline([x1, y1, x2, y2])
    var color = randomColor()
    renderedLine.attr({
        "stroke-width": 5,
        stroke: color
    })
    renderedLine.drag()
    var endPoint1 = s.circle(x1, y1, 5)
    var endPoint2 = s.circle(x2, y2, 5)
    var group = s.g(renderedLine, endPoint1, endPoint2)
    group.attr({ fill: color })
    return group
}

/****************************************
* Renders a polygon where the line is a list of points, not coord objects
*****************************************/
function renderPolygonPointList(s, line) {
    if(DEBUG) console.log("rendering polygon", line);
    var polygon = s.polygon(line)
    var color = randomColor()
    polygon.attr({
        "stroke-width": 5,
        fill: color
    })
    polygon.drag()
    return polygon
}


///// SMALL HELPER FUNCITONS
//////////////////////////////////////////////////

function genPoint([xmin, xmax], [ymin, ymax]) {
    x = Math.floor(Math.random() * (xmax - xmin)) + xmin;
    y = Math.floor(Math.random() * (ymax - ymin)) + ymin;
    return {x, y}
}
function genLine(xRange, yRange) {
    pt1 = genPoint(xRange, yRange)
    pt2 = genPoint(xRange, yRange)
    return {pt1, pt2}
}
/****************************************
* renders a point in list form
*****************************************/
function renderPoint(s, pt) {
    [x, y] = pt
    var renderedPoint = s.circle(x, y, 5)
    renderedPoint.attr({ fill: "red" })
    return renderedPoint
}
/****************************************
* List of points to list of coordinate objects
*****************************************/
function toCoords(list) {
    var ptList = []
    for(var i = 0; i < list.length; i+=2){
        var pts = {}
        pts.x = list[i]
        pts.y = list[i+1]
        ptList.push(pts)
    }
    return ptList
}

/****************************************
* https://www.paulirish.com/2009/random-hex-color-code-snippets/
*****************************************/
function randomColor() {
    return '#' + Math.floor(Math.random()*16777215).toString(16);
}

// Main function invokation
$(document).ready(function () {
    main();
});
