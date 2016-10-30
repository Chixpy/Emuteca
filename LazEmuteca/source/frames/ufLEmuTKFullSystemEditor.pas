unit ufLEmuTKFullSystemEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, ComCtrls,
  ufCHXPropEditor,
  ucEmuteca, ucEmutecaSystem,
  ufEmutecaSystemEditor, ufEmutecaSystemImgEditor;

type

  { TfmLEmuTKFullSystemEditor }

  TfmLEmuTKFullSystemEditor = class(TfmCHXPropEditor)
    pcProperties: TPageControl;
  private
    FEmuteca: cEmuteca;
    FSysEditor: TfmEmutecaSystemEditor;
    FSysInfoEditor: TfmSystemInfoEditor;
    FSystem: cEmutecaSystem;
    procedure SetEmuteca(AValue: cEmuteca);
    procedure SetSystem(AValue: cEmutecaSystem);
    { private declarations }

  protected
    property SysEditor: TfmEmutecaSystemEditor read FSysEditor;
    property SysInfoEditor: TfmSystemInfoEditor read FSysInfoEditor;

  public
    procedure SaveData; override;
    procedure LoadData; override;
    { public declarations }
        property Emuteca: cEmuteca read FEmuteca write SetEmuteca;
    property System: cEmutecaSystem read FSystem write SetSystem;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmLEmuTKFullSystemEditor }

procedure TfmLEmuTKFullSystemEditor.SetEmuteca(AValue: cEmuteca);
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

procedure TfmLEmuTKFullSystemEditor.SetSystem(AValue: cEmutecaSystem);
begin
  if FSystem = AValue then Exit;
  FSystem := AValue;
  SysEditor.System := Self.System;
end;

procedure TfmLEmuTKFullSystemEditor.SaveData;
begin
    SysEditor.SaveData;
end;

procedure TfmLEmuTKFullSystemEditor.LoadData;
begin
  SysEditor.LoadData;
end;

constructor TfmLEmuTKFullSystemEditor.Create(TheOwner: TComponent);

  procedure CreatePages;
  var
    aTabSheet: TTabSheet;
  begin
    aTabSheet := pcProperties.AddTabSheet;
    FSysEditor := TfmEmutecaSystemEditor.Create(aTabSheet);
    SysEditor.Parent := aTabSheet;
    SysEditor.SaveButtons := False;
    SysEditor.Align := alClient;

    aTabSheet := pcProperties.AddTabSheet;
    FSysInfoEditor := TfmSystemInfoEditor.Create(aTabSheet);
    SysInfoEditor.Parent := aTabSheet;
    //    SysInfoEditor.SaveButtons := False;
    SysInfoEditor.Align := alClient;
  end;

begin
  inherited Create(TheOwner);

  CreatePages;
end;

destructor TfmLEmuTKFullSystemEditor.Destroy;
begin
  inherited Destroy;
end;

end.

