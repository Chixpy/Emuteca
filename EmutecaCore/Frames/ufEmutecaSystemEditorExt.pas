unit ufEmutecaSystemEditorExt;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ComCtrls, ExtCtrls, StdCtrls,
  Buttons, ucEmuteca, ucEmutecaSystem, ufEmutecaSystemEditor,
  ufEmutecaSystemInfoEditor;

type

  { TfmEmutecaSystemEditorExt }

  TfmEmutecaSystemEditorExt = class(TFrame)
    bCancel: TBitBtn;
    bSave: TBitBtn;
    PageControl1: TPageControl;
    Panel1: TPanel;
    procedure bCancelClick(Sender: TObject);
    procedure bSaveClick(Sender: TObject);
  private
    FEmuteca: cEmuteca;
    FIconsIni: string;
    FSysEditor: TfmEmutecaSystemEditor;
    FSysInfoEditor: TfmSystemInfoEditor;
    FSystem: cEmutecaSystem;
    procedure SetEmuteca(AValue: cEmuteca);
    procedure SetIconsIni(AValue: string);
    procedure SetSysEditor(AValue: TfmEmutecaSystemEditor);
    procedure SetSysInfoEditor(AValue: TfmSystemInfoEditor);
    procedure SetSystem(AValue: cEmutecaSystem);

  protected
    property SysEditor: TfmEmutecaSystemEditor
      read FSysEditor write SetSysEditor;
    property SysInfoEditor: TfmSystemInfoEditor
      read FSysInfoEditor write SetSysInfoEditor;
  public
    { public declarations }
    property IconsIni: string read FIconsIni write SetIconsIni;
    property Emuteca: cEmuteca read FEmuteca write SetEmuteca;
    property System: cEmutecaSystem read FSystem write SetSystem;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmEmutecaSystemEditorExt }

procedure TfmEmutecaSystemEditorExt.bSaveClick(Sender: TObject);
begin
  SysEditor.SaveData;
end;

procedure TfmEmutecaSystemEditorExt.bCancelClick(Sender: TObject);
begin
  SysEditor.UpdateData;
end;

procedure TfmEmutecaSystemEditorExt.SetEmuteca(AValue: cEmuteca);
begin
  if FEmuteca = AValue then
    Exit;
  FEmuteca := AValue;


  if not Assigned(Emuteca) then
  begin
    SysEditor.EmuManager := nil;
    SysEditor.Config := nil;
  end
  else
  begin
    SysEditor.EmuManager := Emuteca.EmulatorManager;
    SysEditor.Config := Emuteca.Config;
  end;
end;

procedure TfmEmutecaSystemEditorExt.SetIconsIni(AValue: string);
begin
  if FIconsIni = AValue then
    Exit;
  FIconsIni := AValue;
end;

procedure TfmEmutecaSystemEditorExt.SetSysEditor(
  AValue: TfmEmutecaSystemEditor);
begin
  if FSysEditor = AValue then
    Exit;
  FSysEditor := AValue;
end;

procedure TfmEmutecaSystemEditorExt.SetSysInfoEditor(AValue:
  TfmSystemInfoEditor);
begin
  if FSysInfoEditor = AValue then
    Exit;
  FSysInfoEditor := AValue;
end;

procedure TfmEmutecaSystemEditorExt.SetSystem(AValue: cEmutecaSystem);
begin
  if FSystem=AValue then Exit;
  FSystem:=AValue;
  SysEditor.System := Self.System;
end;

constructor TfmEmutecaSystemEditorExt.Create(TheOwner: TComponent);

  procedure CreatePages;
  var
    aTabSheet: TTabSheet;
  begin
    aTabSheet := PageControl1.AddTabSheet;
    FSysEditor := TfmEmutecaSystemEditor.Create(aTabSheet);
    SysEditor.Parent := aTabSheet;
    SysEditor.Align := alClient;
    SysEditor.SaveButtons := False;

    aTabSheet := PageControl1.AddTabSheet;
    FSysInfoEditor := TfmSystemInfoEditor.Create(aTabSheet);
    SysInfoEditor.Parent := aTabSheet;
    SysInfoEditor.Align := alClient;
  end;

begin
  inherited Create(TheOwner);

  CreatePages;
end;

destructor TfmEmutecaSystemEditorExt.Destroy;
begin
  inherited Destroy;
end;

end.
