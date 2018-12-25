import * as React from "react";
import { toScreen, toHex } from "../Util";
import draggable from "../Draggable";
import { IGPIPropsDraggable } from "../types";
import RoughWrapper from "./RoughWrapper";

class Circle extends React.Component<IGPIPropsDraggable> {
  public render() {
    const { shape } = this.props;
    const { dx, dy, onClick } = this.props;
    const { canvasSize } = this.props;
    const [x, y] = toScreen([shape.x.contents, shape.y.contents], canvasSize);
    const fillColor = toHex(shape.color.contents);
    const fillAlpha = shape.color.contents[3];
    // const strokeColor = toHex(shape.strokeColor.contents);
    // const strokeAlpha = shape.strokeColor.contents[3];
    // const thickness = shape.strokeWidth.contents;
    const rw = new RoughWrapper();
    const html = rw.getInnerHTML(
      rw.roughSvg.circle(0, 0, shape.r.contents * 2, {
        roughness: rw.roughness,
        fill: fillColor
      })
    );
    return (
      <g
        transform={`translate(${x - dx},${y + dy})`}
        pointerEvents="bounding-box"
        dangerouslySetInnerHTML={{ __html: html }}
        fillOpacity={fillAlpha}
        onMouseDown={onClick}
      />
    );
  }
}
export default draggable(Circle);
