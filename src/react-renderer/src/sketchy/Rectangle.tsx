import * as React from "react";
import { toScreen, toHex } from "../Util";
import draggable from "../Draggable";
import { IGPIPropsDraggable } from "../types";
import RoughWrapper from "./RoughWrapper";
class Rectangle extends React.Component<IGPIPropsDraggable> {
  public render() {
    const { shape } = this.props;
    const { canvasSize } = this.props;
    const { dy, dx, onClick } = this.props;
    const [x, y] = toScreen([shape.x.contents, shape.y.contents], canvasSize);
    const color = toHex(shape.color.contents);
    const alpha = shape.color.contents[3];
    const rw = new RoughWrapper();
    const html = rw.getInnerHTML(
      rw.roughSvg.rectangle(0, 0, shape.sizeX.contents, shape.sizeY.contents, {
        fill: color,
        roughness: rw.roughness
      })
    );
    return (
      <g
        x={x - shape.sizeX.contents / 2 - dx}
        y={y - shape.sizeY.contents / 2 + dy}
        dangerouslySetInnerHTML={{ __html: html }}
        fillOpacity={alpha}
        pointerEvents="bounding-boc"
        onMouseDown={onClick}
      />
    );
  }
}
export default draggable(Rectangle);
