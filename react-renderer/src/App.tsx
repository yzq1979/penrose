import * as React from "react";
import Log from "./Log";
import Canvas from "./Canvas";
import ButtonBar from "./ButtonBar";
import { ILayer } from "./types";
import { Step, Resample, converged, initial } from "./packets";
import { Protocol, ConnectionStatus } from "./Protocol";
import { evalTranslation, decodeState } from "./Evaluator";
import { step, OptimizerWorker } from "./Optimizer";
import { unwatchFile } from "fs";
import { collectLabels } from "./utills/CollectLabels";

interface ICanvasState {
  data: State | undefined; // NOTE: if the backend is not connected, data will be undefined, TODO: rename this field
  autostep: boolean;
  layers: ILayer[];
  processedInitial: boolean;
  penroseVersion: string;
}
const socketAddress = "ws://localhost:9160";

class App extends React.Component<any, ICanvasState> {
  public readonly state: ICanvasState = {
    data: undefined,
    autostep: false,
    processedInitial: false, // TODO: clarify the semantics of this flag
    layers: [
      { layer: "polygon", enabled: false },
      { layer: "bbox", enabled: false },
    ],
    penroseVersion: "",
  };
  public readonly canvas = React.createRef<Canvas>();
  public readonly buttons = React.createRef<ButtonBar>();
  public optimizerWorker: OptimizerWorker;
  public protocol: Protocol;
  public onConnectionStatus = (conn: ConnectionStatus) => {
    Log.info(`Connection status: ${conn}`);
  };
  public onVersion = (version: string) => {
    this.setState({ penroseVersion: version });
  };
  public onCanvasState = async (canvasState: State, _: any) => {
    // HACK: this will enable the "animation" that we normally expect
    // await new Promise((r) => setTimeout(r, 1));
    const labeledShapes: any = await collectLabels(canvasState.shapes);
    const newState = { ...canvasState, shapes: labeledShapes };
    await this.setState({
      data: newState,
      processedInitial: true,
    });

    const { autostep } = this.state;
    if (autostep && !converged(newState)) {
      await this.step(newState);
    } else {
      await this.setState({
        data: newState,
        processedInitial: true,
      });
    }
  };
  public downloadSVG = () => {
    if (this.canvas.current !== null) {
      this.canvas.current.downloadSVG();
    }
  };
  public downloadPDF = () => {
    if (this.canvas.current !== null) {
      this.canvas.current.downloadPDF();
    }
  };
  public autoStepToggle = async () => {
    await this.setState({ autostep: !this.state.autostep });
    if (this.state.autostep && this.state.processedInitial) {
      this.step();
    }
  };
  public stepState = async (state: State) => {
    const newState = await this.optimizerWorker.step(state!, 2);
    // await onUpdate({ ...newState, shapes: labeledShapes }); // callback for React state update
  };
  public step = async (state = this.state.data!) => {
    // this.protocol.sendPacket(Step(1, this.state.data));
    await this.stepState(state);
  };

  public resample = async () => {
    const NUM_SAMPLES = 50;
    await this.setState({ processedInitial: false });
    this.protocol.sendPacket(Resample(NUM_SAMPLES, this.state.data));
  };
  public toggleLayer = (layerName: string) => {
    this.setState({
      layers: this.state.layers.map(({ layer, enabled }: ILayer) => {
        if (layerName === layer) {
          return { layer, enabled: !enabled };
        }
        return { layer, enabled };
      }),
    });
  };

  public async componentDidMount() {
    this.protocol = new Protocol(socketAddress, {
      onConnectionStatus: this.onConnectionStatus,
      onVersion: this.onVersion,
      onCanvasState: this.onCanvasState,
      onError: console.warn,
      kind: "renderer",
    });

    this.protocol.setupSockets();
    this.optimizerWorker = new OptimizerWorker(this.onCanvasState);
  }

  public updateData = async (data: any) => {
    await this.setState({ data: { ...data } });
    if (this.state.autostep) {
      this.stepState(data);
    }
  };

  public render() {
    const { data, layers, autostep, penroseVersion } = this.state;
    return (
      <div className="App" style={{ height: "100vh" }}>
        <ButtonBar
          downloadPDF={this.downloadPDF}
          downloadSVG={this.downloadSVG}
          autostep={autostep}
          step={this.step}
          autoStepToggle={this.autoStepToggle}
          resample={this.resample}
          converged={data ? converged(data) : false} // TODO: what should be the default value here?
          initial={data ? initial(data) : false}
          toggleLayer={this.toggleLayer}
          layers={layers}
          ref={this.buttons}
        />
        <Canvas
          data={data}
          updateData={this.updateData}
          lock={false}
          layers={layers}
          ref={this.canvas}
          penroseVersion={penroseVersion}
        />
      </div>
    );
  }
}

export default App;
