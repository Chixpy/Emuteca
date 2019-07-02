unit ufEmutecaEmulatorAdvParamsEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons, ActnList,
  // CHX units
  uCHXStrUtils, uCHXDlgUtils,
  // CHX frames
  ufCHXPropEditor,
  // Emuteca Core abstract
  uaEmutecaCustomEmu,
  // Emuteca Core classes
  ucEmutecaEmulator;

type

  { TfmEmutecaEmulatorAdvParamsEditor }

  TfmEmutecaEmulatorAdvParamsEditor = class(TfmCHXPropEditor)
    eCoreIDParamFmt: TEdit;
    eMultiEmuID: TEdit;
    gbxCoreID: TGroupBox;
    gbxExtParams: TGroupBox;
    gbxExtraSoftParameters: TGroupBox;
    lCoreParamFmt: TLabel;
    lMultiEmuID: TLabel;
    mExtensionParameters: TMemo;
    mExtraParameters: TMemo;
    Splitter1: TSplitter;

  private
    FEmulator: cEmutecaEmulator;
    procedure SetEmulator(const AValue: cEmutecaEmulator);

  protected
    procedure DoClearFrameData;
    procedure DoLoadFrameData;
    procedure DoSaveFrameData;

  public
    property Emulator: cEmutecaEmulator read FEmulator write SetEmulator;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmEmutecaEmulatorAdvParamsEditor }

procedure TfmEmutecaEmulatorAdvParamsEditor.SetEmulator(
  const AValue: cEmutecaEmulator);
begin
  if FEmulator = AValue then
    Exit;
  FEmulator := AValue;
  LoadFrameData;
end;

procedure TfmEmutecaEmulatorAdvParamsEditor.DoClearFrameData;
begin
  eMultiEmuID.Clear;
  eCoreIDParamFmt.Clear;
  mExtensionParameters.Clear;
  mExtraParameters.Clear;
end;

procedure TfmEmutecaEmulatorAdvParamsEditor.DoLoadFrameData;
begin
  ClearFrameData;

  Enabled := assigned(Emulator);

  if not Enabled then
    Exit;

  eMultiEmuID.Text := Emulator.CoreIDKey;
  eCoreIDParamFmt.Text := Emulator.CoreIDParamFormat;
  mExtensionParameters.Lines.Assign(Emulator.ExtensionParamFormat);
  mExtraParameters.Lines.Assign(Emulator.ExtraParamFormat);
end;

procedure TfmEmutecaEmulatorAdvParamsEditor.DoSaveFrameData;
begin
  Emulator.CoreIDKey := eMultiEmuID.Text;
  Emulator.CoreIDParamFormat := eCoreIDParamFmt.Text;
  Emulator.ExtensionParamFormat.Assign(mExtensionParameters.Lines);
  Emulator.ExtraParamFormat.Assign(mExtraParameters.Lines);
end;

constructor TfmEmutecaEmulatorAdvParamsEditor.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  OnClearFrameData := @DoClearFrameData;
  OnLoadFrameData := @DoLoadFrameData;
  OnSaveFrameData := @DoSaveFrameData;
end;

destructor TfmEmutecaEmulatorAdvParamsEditor.Destroy;
begin
  inherited Destroy;
end;

end.
