unit ufLEmuTKExportData;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, ActnList, StdCtrls, EditBtn,
  ufCHXPropEditor,
  ucEmuteca, ucEmutecaSystem,
  ufEmutecaSystemCBXOld;

type

  { TfmLEmuTKExportData }

  TfmLEmuTKExportData = class(TfmCHXPropEditor)
    eExportFile: TFileNameEdit;
    gbxSelectSystem: TGroupBox;
    lSelectFile: TLabel;
  private
    FEmuteca: cEmuteca;
    FfmSystemCBX: TfmEmutecaSystemCBX;
    procedure SetEmuteca(AValue: cEmuteca);

  protected
    property fmSystemCBX: TfmEmutecaSystemCBX read FfmSystemCBX;

  public
    property Emuteca: cEmuteca read FEmuteca write SetEmuteca;

    function SelectSystem(aSystem: cEmutecaSystem): boolean;
    //< Select a system

    procedure ClearData; override;
    procedure LoadData; override;
    procedure SaveData; override;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmLEmuTKExportData }

procedure TfmLEmuTKExportData.SetEmuteca(AValue: cEmuteca);
begin
  if FEmuteca = AValue then Exit;
  FEmuteca := AValue;
  if Assigned(Emuteca) then
  begin
    fmSystemCBX.SystemList := Emuteca.SystemManager.FullList;
  end
  else
  begin
    fmSystemCBX.SystemList := nil;
  end;
  Enabled := Assigned(Emuteca);
end;

procedure TfmLEmuTKExportData.ClearData;
begin

end;

function TfmLEmuTKExportData.SelectSystem(aSystem: cEmutecaSystem): boolean;
begin
  if Assigned(aSystem) then
    eExportFile.FileName := aSystem.FileName + '.edb'
  else
    eExportFile.FileName := '';

  Result := not (eExportFile.FileName = '')
end;

constructor TfmLEmuTKExportData.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

    FfmSystemCBX := TfmEmutecaSystemCBX.Create(gbxSelectSystem);
    fmSystemCBX.Align := alTop;
    fmSystemCBX.OnSelectSystem := @SelectSystem;
    fmSystemCBX.Parent := gbxSelectSystem;
end;

destructor TfmLEmuTKExportData.Destroy;
begin
  inherited Destroy;
end;

procedure TfmLEmuTKExportData.LoadData;
begin

end;

procedure TfmLEmuTKExportData.SaveData;
begin
  if Assigned(fmSystemCBX.SelectedSystem) then
  begin
  //  Emuteca.SoftManager.SaveToFileIni(eExportFile.FileName, True);
  end
  else
  begin
    // TODO: Little test... :-P
  end;

end;

end.

