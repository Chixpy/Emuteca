unit ufESMSoftList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, VirtualTrees, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, ComCtrls,
  ufCHXTagTree,
  ucEmuteca, ucEmutecaGroup, ucEmutecaSoftware, ucEmutecaSystem,
  ufEmutecaSystemCBX;

type

  { TfrmESMSoftList }

  TfrmESMSoftList = class(TForm)
    PageControl1: TPageControl;
    pBottom: TPanel;
    pButtons: TPanel;
    pMiddle: TPanel;
    pTop: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    procedure FormCreate(Sender: TObject);

  private
    FEmuteca: cEmuteca;

    // Frames
    fmEmutecaSystemCBX: TfmEmutecaSystemCBX;
    fmCHXTagTree: TfmCHXTagTree;

    procedure SetEmuteca(AValue: cEmuteca);

  protected
    procedure CheckTags(aList: TStrings);
    function SelectSystem(aSystem: cEmutecaSystem): boolean;
    function SelectGroup(aGroup: cEmutecaGroup): boolean;
    function SelectSoftware(aSoftware: cEmutecaSoftware): boolean;

  public
    { public declarations }
    property Emuteca: cEmuteca read FEmuteca write SetEmuteca;
  end;

var
  frmESMSoftList: TfrmESMSoftList;

implementation

{$R *.lfm}

{ TfrmESMSoftList }

procedure TfrmESMSoftList.FormCreate(Sender: TObject);
  procedure CreateFrames;
  var
    aTabSheet: TTabSheet;
  begin
    // Better create frames in code while developing...
    //   IDE has many problems updating inherited properties

    // Creating and Setting the System ComboBox
    fmEmutecaSystemCBX := TfmEmutecaSystemCBX.Create(pMiddle);
    fmEmutecaSystemCBX.Align := alTop;
    fmEmutecaSystemCBX.OnSelectSystem := @SelectSystem;
    fmEmutecaSystemCBX.Parent := pMiddle;


    // Creating and Setting Tags
    aTabSheet := PageControl1.AddTabSheet;
    fmCHXTagTree := TfmCHXTagTree.Create(aTabSheet);
    aTabSheet.Caption := fmCHXTagTree.Caption;  // TODO: Add Caption
    fmCHXTagTree.OnCheckChange := @CheckTags;
    fmCHXTagTree.Parent := aTabSheet;
  end;
begin
  CreateFrames;
end;

procedure TfrmESMSoftList.SetEmuteca(AValue: cEmuteca);
begin
  if FEmuteca = AValue then Exit;
  FEmuteca := AValue;

  if assigned(Emuteca) then
  begin
    fmEmutecaSystemCBX.SystemList := Emuteca.SystemManager.EnabledList;
  end
  else
  begin
    fmEmutecaSystemCBX.SystemList := nil;
  end;
end;

procedure TfrmESMSoftList.CheckTags(aList: TStrings);
begin

end;

function TfrmESMSoftList.SelectGroup(aGroup: cEmutecaGroup): boolean;
begin
  Result := False;
end;

function TfrmESMSoftList.SelectSoftware(aSoftware: cEmutecaSoftware): boolean;
begin
  Result := False;
end;

function TfrmESMSoftList.SelectSystem(aSystem: cEmutecaSystem): boolean;
begin
  Result := False;
end;

end.
