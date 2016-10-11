unit ufESMSoftList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, VirtualTrees, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, ComCtrls,
  ufTagTree,
  ucEmuteca, ucEmutecaParent, ucEmutecaSoftware, ucEmutecaSystem,
  ufEmutecaSystemCBX, ufEmutecaChkSoftList, ufEmutecaParentList;

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
    fmEmutecaParentList: TfmEmutecaParentList;
    fmEmutecaSoftList: TfmEmutecaChkSoftList;
    fmCHXTagTree: TfmTagTree;

    procedure SetEmuteca(AValue: cEmuteca);

  protected
    procedure CheckTags(aList: TStrings);
    procedure SelectParent(const aParent: cEmutecaParent);
    procedure SelectSoftware(const aSoftware: cEmutecaSoftware);
    function SelectSystem(aSystem: cEmutecaSystem): boolean;

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
    fmEmutecaSystemCBX.Parent := pMiddle;
    fmEmutecaSystemCBX.Align := alTop;
    fmEmutecaSystemCBX.OnSelectSystem := @Self.SelectSystem;


    // Creating and setting the parent list frame
    fmEmutecaParentList := TfmEmutecaParentList.Create(pTop);
    fmEmutecaParentList.Parent := pTop;
    fmEmutecaSystemCBX.Align := alClient;
    fmEmutecaParentList.OnItemSelect := @Self.SelectParent;


    // Creating and Setting the software list frame
    fmEmutecaSoftList := TfmEmutecaChkSoftList.Create(pBottom);
    fmEmutecaSoftList.Parent := pBottom;
   // fmEmutecaSoftList.OnItemSelect := @Self.SelectSoftware;



        // Creating and Setting Tags
    aTabSheet := PageControl1.AddTabSheet;
    fmCHXTagTree := TfmTagTree.Create(aTabSheet);
    aTabSheet.Caption := fmCHXTagTree.Caption;  {TODO: Add Caption}
    fmCHXTagTree.Parent := aTabSheet;
    fmCHXTagTree.Folder := Emuteca.Config.TagSubFolder;
    fmCHXTagTree.OnCheckChange := @self.CheckTags;
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
    fmEmutecaSystemCBX.SystemList := Emuteca.SystemManager.VisibleList;
    fmEmutecaParentList.ParentList := Emuteca.ParentManager.FullList;
    fmEmutecaSoftList.SoftList := Emuteca.SoftManager.EnabledList;
  end
  else
  begin
    fmEmutecaSystemCBX.SystemList := nil;
    fmEmutecaParentList.ParentList := nil;
    fmEmutecaSoftList.SoftList := nil;
  end;
end;

procedure TfrmESMSoftList.CheckTags(aList: TStrings);
begin

end;

procedure TfrmESMSoftList.SelectParent(const aParent: cEmutecaParent);
begin

end;

procedure TfrmESMSoftList.SelectSoftware(const aSoftware: cEmutecaSoftware);
begin

end;

function TfrmESMSoftList.SelectSystem(aSystem: cEmutecaSystem): boolean;
begin

end;

end.
