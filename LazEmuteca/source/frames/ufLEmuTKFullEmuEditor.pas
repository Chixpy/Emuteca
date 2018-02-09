unit ufLEmuTKFullEmuEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, ActnList, ComCtrls, StdCtrls, LCLIntf, LazFileUtils,
  uCHXStrUtils,
  ufCHXPropEditor,
  ucEmutecaEmulator,
  ufEmutecaEmulatorEditor;

type

  { TfmLEmuTKFullEmuEditor }

  TfmLEmuTKFullEmuEditor = class(TfmCHXPropEditor)
    actOpenEmulatorFolder: TAction;
    pcProperties: TPageControl;
    ToolBar1: TToolBar;
    bOpenEmulatorFolder: TToolButton;
    procedure actOpenEmulatorFolderExecute(Sender: TObject);
  private
    FEmuEditor: TfmEmutecaEmulatorEditor;
    FEmulator: cEmutecaEmulator;
    FSHA1Folder: string;
    procedure SetEmulator(AValue: cEmutecaEmulator);
    procedure SetSHA1Folder(AValue: string);
    { private declarations }

  protected
    property EmuEditor: TfmEmutecaEmulatorEditor read FEmuEditor;

    procedure DoClearFrameData;
    procedure DoLoadFrameData;
    procedure DoSaveFrameData;

  public
    { public declarations }
    property Emulator: cEmutecaEmulator read FEmulator write SetEmulator;

    property SHA1Folder: string read FSHA1Folder write SetSHA1Folder;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmLEmuTKFullEmuEditor }

procedure TfmLEmuTKFullEmuEditor.actOpenEmulatorFolderExecute(Sender: TObject);
var
  aFolder: string;
begin
  if not Assigned(Emulator) then
    Exit;

  aFolder := ExtractFilePath(Emulator.ExeFile);
  if not DirectoryExistsUTF8(aFolder) then
    Exit;

  OpenDocument(aFolder);
end;

procedure TfmLEmuTKFullEmuEditor.SetEmulator(AValue: cEmutecaEmulator);
begin
  if FEmulator = AValue then
    Exit;
  FEmulator := AValue;
  EmuEditor.Emulator := Emulator;

  LoadFrameData;
end;

procedure TfmLEmuTKFullEmuEditor.SetSHA1Folder(AValue: string);
begin
  FSHA1Folder := SetAsFolder(AValue);
  //fmEmuImgEditor.SHA1Folder := SHA1Folder;
end;

procedure TfmLEmuTKFullEmuEditor.DoClearFrameData;
begin

end;

procedure TfmLEmuTKFullEmuEditor.DoLoadFrameData;
begin
  Enabled := Assigned(Emulator);

  if not Enabled then
  begin
    ClearFrameData;
    Exit;
  end;
end;

procedure TfmLEmuTKFullEmuEditor.DoSaveFrameData;
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
    EmuEditor.Align := alClient;
    EmuEditor.Parent := aTabSheet;

    aTabSheet := pcProperties.AddTabSheet;
  end;

begin
  inherited Create(TheOwner);

  CreatePages;

  OnClearFrameData := @DoClearFrameData;
  OnLoadFrameData := @DoLoadFrameData;
  OnSaveFrameData := @DoSaveFrameData;
end;

destructor TfmLEmuTKFullEmuEditor.Destroy;
begin
  inherited Destroy;
end;

end.
