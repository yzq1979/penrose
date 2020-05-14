export interface IGPIProps<T = any> {
  shape: T;
  canvasSize: ICanvasSize;
}

export interface IGPIPropsDraggable<T = any> extends IGPIProps<T> {
  ctm: DOMMatrix;
  onClick(e: React.MouseEvent<any>): void;
  dragEvent?(id: string, dy: number, dx: number): void;
}

export interface ILayerProps {
  shapes: Shape[];
  debugData: any[];
  ctm: DOMMatrix;
  canvasSize: ICanvasSize;
}

export interface ILayer {
  layer: string;
  enabled: boolean;
}

export interface ICanvasSize {
  width: number;
  height: number;
}
