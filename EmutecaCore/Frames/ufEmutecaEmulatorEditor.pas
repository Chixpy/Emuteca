unit ufEmutecaEmulatorEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, EditBtn, Spin,
  ExtCtrls, Buttons, ActnList, Menus,
  uCHXStrUtils,
  ucEmutecaEmulator;

resourcestring
  rsSelectEmulator = 'Select an Emulator';

type

  { TfmEmutecaEmulatorEditor }

  TfmEmutecaEmulatorEditor = class(TFrame)
    actCancel: TAction;
    actWFEmulator: TAction;
    actWFROM: TAction;
    actSave: TAction;
    ActionList: TActionList;
    bCancel: TBitBtn;
    bOk: TBitBtn;
    eExePath: TFileNameEdit;
    eExitCode: TSpinEdit;
    eName: TEdit;
    eParameters: TEdit;
    eWorkingFolder: TDirectoryEdit;
    lExePath: TLabel;
    lExitCode: TLabel;
    lExtensions: TLabel;
    lID: TLabel;
    lName: TLabel;
    lParameters: TLabel;
    lWorkingFolder: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    mExtensions: TMemo;
    Panel1: TPanel;
    pBottom: TPanel;
    pmWorkingFolder: TPopupMenu;
    pWFolder: TPanel;
    SpeedButton1: TSpeedButton;
    procedure actCancelExecute(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
    procedure actWFEmulatorExecute(Sender: TObject);
    procedure actWFROMExecute(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);

  private
    { private declarations }
    FEmulator: cEmutecaEmulator;
    procedure SetEmulator(AValue: cEmutecaEmulator);

  protected
    procedure UpdateData;
    procedure ClearData;

  public
    { public declarations }
    property Emulator: cEmutecaEmulator read FEmulator write SetEmulator;

  end;

implementation

{$R *.lfm}

{ TfmEmutecaEmulatorEditor }

procedure TfmEmutecaEmulatorEditor.actSaveExecute(Sender: TObject);
begin
  Emulator.EmulatorName := eName.Text;
  Emulator.ExeFile := eExePath.Text;
  Emulator.WorkingFolder := eWorkingFolder.Text;
  Emulator.Parameters := eParameters.Text;
  Emulator.FileExt.Assign(mExtensions.Lines);
  Emulator.ExitCode := eExitCode.Value;
end;

procedure TfmEmutecaEmulatorEditor.actWFEmulatorExecute(Sender: TObject);
begin
  eWorkingFolder.Text := kEmutecaEmuDirKey;
end;

procedure TfmEmutecaEmulatorEditor.actWFROMExecute(Sender: TObject);
begin
  eWorkingFolder.Text := kEmutecaRomDirKey;
end;

procedure TfmEmutecaEmulatorEditor.SpeedButton1Click(Sender: TObject);
begin
  pmWorkingFolder.PopUp;
end;

procedure TfmEmutecaEmulatorEditor.actCancelExecute(Sender: TObject);
begin
  UpdateData;
end;

procedure TfmEmutecaEmulatorEditor.SetEmulator(AValue: cEmutecaEmulator);
begin
  if FEmulator = AValue then
    Exit;
  FEmulator := AValue;
  UpdateData;
end;

procedure TfmEmutecaEmulatorEditor.UpdateData;
begin
  ClearData;
  if not assigned(Emulator) then
    Exit;

  lID.Caption := Emulator.ID;
  eName.Text := Emulator.EmulatorName;
  eExePath.Text := SysPath(Emulator.ExeFile);
  eWorkingFolder.Text := SysPath(Emulator.WorkingFolder);
  eParameters.Text := Emulator.Parameters;
  mExtensions.Lines.Assign(Emulator.FileExt);
  eExitCode.Value := Emulator.ExitCode;
end;

procedure TfmEmutecaEmulatorEditor.ClearData;
begin
  lID.Caption := rsSelectEmulator;
  eName.Clear;
  eExePath.Clear;
  eWorkingFolder.Clear;
  eParameters.Clear;
  mExtensions.Clear;
  eExitCode.Value := 0;
end;

end.
