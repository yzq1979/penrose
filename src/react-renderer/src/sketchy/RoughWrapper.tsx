import rough from "roughjs";
class RoughWrapper {
  public readonly roughness = 1;
  public svg = (document.createElement("svg") as any) as SVGSVGElement;
  public roughSvg = rough.svg(this.svg);
  public getInnerHTML = (g: SVGGElement | Promise<SVGGElement>) => {
    this.svg.appendChild((g as any) as SVGGElement);
    const html = this.svg.innerHTML;
    this.remove();
    return html;
  };
  public remove = () => {
    this.svg.remove();
  };
}
export default RoughWrapper;
