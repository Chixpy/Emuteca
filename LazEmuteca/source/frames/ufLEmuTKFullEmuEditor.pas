unit ufLEmuTKFullEmuEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, ActnList, ComCtrls,
  ufCHXPropEditor,
  ucEmutecaEmulator,
  ufEmutecaEmulatorEditor;

type

  { TfmLEmuTKFullEmuEditor }

  TfmLEmuTKFullEmuEditor = class(TfmCHXPropEditor)
    pcProperties: TPageControl;
  private
    FEmuEditor: TfmEmutecaEmulatorEditor;
    FEmulator: cEmutecaEmulator;
    procedure SetEmulator(AValue: cEmutecaEmulator);
    { private declarations }

  protected
    property EmuEditor: TfmEmutecaEmulatorEditor read FEmuEditor;

    procedure ClearFrameData; override;
    procedure LoadFrameData; override;
  public
    { public declarations }
    property Emulator: cEmutecaEmulator read FEmulator write SetEmulator;

    procedure SaveFrameData; override;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmLEmuTKFullEmuEditor }

procedure TfmLEmuTKFullEmuEditor.SetEmulator(AValue: cEmutecaEmulator);
begin
  if FEmulator = AValue then Exit;
  FEmulator := AValue;
  EmuEditor.Emulator := Emulator;
end;

procedure TfmLEmuTKFullEmuEditor.ClearFrameData;
begin

end;

procedure TfmLEmuTKFullEmuEditor.LoadFrameData;
begin

end;

procedure TfmLEmuTKFullEmuEditor.SaveFrameData;
begin
  EmuEditor.SaveFrameData;
end;

constructor TfmLEmuTKFullEmuEditor.Create(TheOwner: TComponent);
  procedure CreatePages;
  var
    aTabSheet: TTabSheet;
  begin
    aTabSheet := pcProperties.AddTabSheet;
    FEmuEditor := TfmEmutecaEmulatorEditor.Create(aTabSheet);
    EmuEditor.SaveButtons := False;
    EmuEditor.Parent := aTabSheet;

    aTabSheet := pcProperties.AddTabSheet;
  end;

begin
  inherited Create(TheOwner);

  CreatePages;
end;

destructor TfmLEmuTKFullEmuEditor.Destroy;
begin
  inherited Destroy;
end;

end.

