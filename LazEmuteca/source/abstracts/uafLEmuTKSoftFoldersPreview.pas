unit uafLEmuTKSoftFoldersPreview;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ufCHXFrame, ufCHXStrLstPreview,
  uaEmutecaCustomSystem, ucEmutecaGroup, ucEmutecaSoftware;

type

  { TfmLEmuTKSoftFoldersPreview }

  TfmLEmuTKSoftFoldersPreview = class(TfmCHXFrame)
    cbxFolderCaption: TComboBox;
    procedure cbxFolderCaptionSelect(Sender: TObject);

  private
    FGroup: cEmutecaGroup;
    FFileExt: TStrings;
    FFileList: TStringList;
    FLastCaption: string;
    FfmListPreview: TfmCHXStrLstPreview;
    FSoftware: cEmutecaSoftware;
    FSystem: caEmutecaCustomSystem;
    procedure SetGroup(AValue: cEmutecaGroup);
    procedure SetFileExt(AValue: TStrings);
    procedure SetLastCaption(AValue: string);

    procedure SetSoftware(AValue: cEmutecaSoftware);
    procedure SetSystem(AValue: caEmutecaCustomSystem);

  protected
    property fmListPreview: TfmCHXStrLstPreview read FfmListPreview;
    property FileList: TStringList read FFileList;

    property System: caEmutecaCustomSystem read FSystem write SetSystem;
    property LastCaption: string read FLastCaption write SetLastCaption;

    procedure UpdateFileList;

    procedure SetListPreview(AValue: TfmCHXStrLstPreview);

    procedure DoClearFrameData;
    procedure DoLoadFrameData;

    procedure CreateListView; virtual; // TODO: abstract; //-> ERROR?
    function GetCaptionList: TStrings; virtual; // TODO: abstract; //-> ERROR?
    function GetFolder: string; virtual; // TODO: abstract; //-> ERROR?

  public
    property Software: cEmutecaSoftware read FSoftware write SetSoftware;
    property Group: cEmutecaGroup read FGroup write SetGroup;

    property FileExt: TStrings read FFileExt write SetFileExt;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmLEmuTKSoftFoldersPreview }

procedure TfmLEmuTKSoftFoldersPreview.cbxFolderCaptionSelect(Sender: TObject);
begin
  if cbxFolderCaption.ItemIndex >=  0 then
    LastCaption := cbxFolderCaption.Items[cbxFolderCaption.ItemIndex];

  UpdateFileList;
end;

procedure TfmLEmuTKSoftFoldersPreview.SetGroup(AValue: cEmutecaGroup);
begin
  if FGroup = AValue then
    Exit;
  FGroup := AValue;

  FSoftware := nil;

  if Assigned(Group) then
    System := Group.CachedSystem;

  UpdateFileList;
end;

procedure TfmLEmuTKSoftFoldersPreview.SetFileExt(AValue: TStrings);
begin
  if FFileExt = AValue then
    Exit;
  FFileExt := AValue;

  UpdateFileList;
end;

procedure TfmLEmuTKSoftFoldersPreview.SetLastCaption(AValue: string);
begin
  if FLastCaption = AValue then
    Exit;
  FLastCaption := AValue;
end;

procedure TfmLEmuTKSoftFoldersPreview.SetListPreview(AValue:
  TfmCHXStrLstPreview);
begin
   if FfmListPreview = AValue then Exit;
   FfmListPreview := AValue;
end;

procedure TfmLEmuTKSoftFoldersPreview.CreateListView;
begin
  // This method must be overrided
end;

function TfmLEmuTKSoftFoldersPreview.GetCaptionList: TStrings;
begin
   // This method must be overrided
   Result := nil;
end;

function TfmLEmuTKSoftFoldersPreview.GetFolder: string;
begin
   // This method must be overrided
   Result := '';
end;

procedure TfmLEmuTKSoftFoldersPreview.SetSoftware(AValue: cEmutecaSoftware);
begin
  if FSoftware = AValue then
    Exit;
  FSoftware := AValue;

  FGroup := nil;
  if Assigned(Software) then
    System := Software.CachedSystem;

  UpdateFileList;
end;

procedure TfmLEmuTKSoftFoldersPreview.SetSystem(AValue: caEmutecaCustomSystem);
begin
  if FSystem = AValue then
    Exit;
  FSystem := AValue;

  LoadFrameData;
end;

procedure TfmLEmuTKSoftFoldersPreview.UpdateFileList;
begin
  if cbxFolderCaption.ItemIndex < 0 then
    Exit;

  fmListPreview.StrList := nil;
  FileList.Clear;

  if Assigned(Software) then
  begin
    Software.SearchAllRelatedFiles(FileList, GetFolder, FileExt, True, True);

    if (FileList.Count = 0) and (not Software.MatchGroupFile) then
    begin
      if Assigned(Software.CachedGroup) then
        Software.CachedGroup.SearchAllRelatedFiles(FileList, GetFolder, FileExt, True, True);
    end;
  end
  else if Assigned(Group) then
  begin
    Group.SearchAllRelatedFiles(FileList, GetFolder, FileExt, True, True);
  end;

  fmListPreview.StrList := FileList;
end;

procedure TfmLEmuTKSoftFoldersPreview.DoClearFrameData;
begin
  cbxFolderCaption.Clear;
end;

procedure TfmLEmuTKSoftFoldersPreview.DoLoadFrameData;
var
  aIndex: integer;
begin
  Enabled := Assigned(System);

  if not Enabled then
  begin
    ClearFrameData;
    Exit;
  end;

  if System.ImageCaptions.Count > 0 then
  begin
    cbxFolderCaption.Items.Assign(GetCaptionList);

    // Restoring last caption
    aIndex := cbxFolderCaption.Items.IndexOf(LastCaption);
    if aIndex <> -1 then
      cbxFolderCaption.ItemIndex := aIndex
    else
      cbxFolderCaption.ItemIndex := 0;
  end
  else
    ClearFrameData;
end;

constructor TfmLEmuTKSoftFoldersPreview.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FFileList := TStringList.Create;

  CreateListView;

  OnClearFrameData := @DoClearFrameData;
  OnLoadFrameData := @DoLoadFrameData;
end;

destructor TfmLEmuTKSoftFoldersPreview.Destroy;
begin
  FileList.Free;

  inherited Destroy;
end;

end.
