unit ufEmutecaTagTree;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ActnList, Menus,
  VirtualTrees, VTHeaderPopup, LazFileUtils,
  // CHX units
  uCHXStrUtils,
  // CHX frames
  ufCHXTagTree,
  // Emuteca classes
  ucEmutecaGroup, ucEmutecaTagsFile;

resourcestring
  rsDefaultFolderName = 'Folder';
  rsDefaultFilename = 'Filename';
  rsCaptionTagsFile = 'Tags File';
  rsCaptionFolderName = 'Folder Name';
  rsWriteTagName = 'Write the tag name';
  rsWriteFolderName = 'Write the folder name';
  rsWriteNewTagName = 'Write the new tag name';
  rsCaptionDeleteFile = 'Delete File?';
  rsCaptionDeleteFolder = 'Delete Folder?';
  rsAskDelete = 'Do you want to delete?' + LineEnding + '%0:s';


type

  { TfmEmutecaTagTree }

  TfmEmutecaTagTree = class(TfmCHXTagTree)
    actAddTagFile: TAction;
    alEmutecaTagTree: TActionList;
    mipmRemoveTagFile: TMenuItem;
    mipmRenameFile: TMenuItem;
    mipmRAddRootFile: TMenuItem;
    mipmRAddRootFolder: TMenuItem;
    mipmDeleteFolder: TMenuItem;
    mipmRenameFolder: TMenuItem;
    pumFile: TPopupMenu;
    pumFolder: TPopupMenu;
    pumRoot: TPopupMenu;
    procedure actAddTagFileExecute(Sender: TObject);
    procedure actAddGroup2TagFileExecute(Sender: TObject);
    procedure actAddRootFileExecute(Sender: TObject);
    procedure actAddRootFolderExecute(Sender: TObject);
    procedure actAddSubFolderExecute(Sender: TObject);
    procedure actRemoveGroupFromFileExecute(Sender: TObject);
    procedure actRenameFileExecute(Sender: TObject);
    procedure VSTGetPopupMenu(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; const P: TPoint;
      var AskParent: boolean; var aPopupMenu: TPopupMenu);

  private
    FCurrentGroup: cEmutecaGroup;
    procedure SetCurrentGroup(const AValue: cEmutecaGroup);

  protected
    procedure AskFile(Node: PVirtualNode);
    procedure AskFolder(Node: PVirtualNode);

  public
    property CurrentGroup: cEmutecaGroup read FCurrentGroup
      write SetCurrentGroup;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmEmutecaTagTree }

procedure TfmEmutecaTagTree.VSTGetPopupMenu(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; const P: TPoint;
  var AskParent: boolean; var aPopupMenu: TPopupMenu);
var
  pData: PCHXTagTreeData;
begin
  pData := Sender.GetNodeData(Node);

  AskParent := False;

  if not Assigned(pData) then
  begin
    aPopupMenu := pumRoot;
  end
  else
  begin
    if pData^.FileName = '' then
      aPopupMenu := pumFolder
    else
      aPopupMenu := pumFile;
  end;
end;

procedure TfmEmutecaTagTree.actAddRootFileExecute(Sender: TObject);
begin
  if not DirectoryExistsUTF8(TagsFolder) then
    Exit;

  AskFile(nil);
end;

procedure TfmEmutecaTagTree.actAddTagFileExecute(Sender: TObject);
var
  CurrNode: PVirtualNode;
  pData: PCHXTagTreeData;
begin
  if not DirectoryExistsUTF8(TagsFolder) then
    Exit;

  CurrNode := VST.GetFirstSelected(False);
  if not assigned(CurrNode) then
    Exit;

  pData := VST.GetNodeData(CurrNode);
  if not assigned(pData) then
    Exit;

  // If file is selected, add to parents folder
  if pData^.FileName <> '' then
    CurrNode := CurrNode^.Parent;

  AskFile(CurrNode);
end;

procedure TfmEmutecaTagTree.actAddGroup2TagFileExecute(Sender: TObject);
var
  CurrNode: PVirtualNode;
  pData: PCHXTagTreeData;
  aTagFile: cEmutecaTagsFile;
begin
  if not Assigned(CurrentGroup) then
    Exit;

  CurrNode := VST.GetFirstSelected(False);
  if not assigned(CurrNode) then
    Exit;

  pData := VST.GetNodeData(CurrNode);
  if not assigned(pData) then
    Exit;

  if not FileExistsUTF8(pData^.Folder + pData^.FileName) then
    Exit;

  aTagFile := cEmutecaTagsFile.Create(nil);
  aTagFile.DefaultFileName := pData^.Folder + pData^.FileName;
  aTagFile.LoadFromFile('');

  if Assigned(CurrentGroup.CachedSystem) then
  begin
    aTagFile.AddGroup(CurrentGroup.CachedSystem.ID, CurrentGroup.ID);
    aTagFile.SaveToFile('', True);
  end;

  aTagFile.Free;
end;

procedure TfmEmutecaTagTree.actAddRootFolderExecute(Sender: TObject);
begin
  if not DirectoryExistsUTF8(TagsFolder) then
    Exit;

  AskFolder(nil);
end;

procedure TfmEmutecaTagTree.actAddSubFolderExecute(Sender: TObject);
var
  CurrNode: PVirtualNode;
  pData: PCHXTagTreeData;
begin
  if not DirectoryExistsUTF8(TagsFolder) then
    Exit;

  CurrNode := VST.GetFirstSelected(False);
  if not assigned(CurrNode) then
    Exit;

  pData := VST.GetNodeData(CurrNode);
  if not assigned(pData) then
    Exit;

  // If file is selected, add to parents folder
  if pData^.FileName <> '' then
    CurrNode := CurrNode^.Parent;

  AskFolder(CurrNode);
end;

procedure TfmEmutecaTagTree.actRemoveGroupFromFileExecute(Sender: TObject);
var
  CurrNode: PVirtualNode;
  pData: PCHXTagTreeData;
  aTagFile: cEmutecaTagsFile;
begin
  if not Assigned(CurrentGroup) then
    Exit;

  CurrNode := VST.GetFirstSelected(False);
  if not assigned(CurrNode) then
    Exit;

  pData := VST.GetNodeData(CurrNode);
  if not assigned(pData) then
    Exit;

  if not FileExistsUTF8(pData^.Folder + pData^.FileName) then
    Exit;

  aTagFile := cEmutecaTagsFile.Create(nil);
  aTagFile.DefaultFileName := pData^.Folder + pData^.FileName;
  aTagFile.LoadFromFile('');

  if Assigned(CurrentGroup.CachedSystem) then
  begin
    aTagFile.RemoveGroup(CurrentGroup.CachedSystem.ID, CurrentGroup.ID);
    aTagFile.SaveToFile('', True);
  end;

  aTagFile.Free;
end;

procedure TfmEmutecaTagTree.actRenameFileExecute(Sender: TObject);
var
  CurrNode: PVirtualNode;
  pData: PCHXTagTreeData;
  FileName: string;
begin
  CurrNode := VST.GetFirstSelected(False);
  if not assigned(CurrNode) then
    Exit;

  pData := VST.GetNodeData(CurrNode);
  if not assigned(pData) then
    Exit;

  FileName := ExtractFileNameOnly(pData^.FileName);
  if not InputQuery(rsCaptionTagsFile, rsWriteNewTagName, FileName) then
    Exit;
  FileName := IncludeTrailingPathDelimiter(pData^.Folder) +
    CleanFileName(FileName, True, False) + '.ini';

  if FileExistsUTF8(FileName) then
    Exit;

  if RenameFileUTF8(pData^.Folder + pData^.FileName, FileName) then
    Pdata^.Title := ExtractFileNameOnly(FileName);
end;

procedure TfmEmutecaTagTree.SetCurrentGroup(const AValue: cEmutecaGroup);
begin
  if FCurrentGroup = AValue then
    Exit;
  FCurrentGroup := AValue;
end;

procedure TfmEmutecaTagTree.AskFile(Node: PVirtualNode);
var
  pData: PCHXTagTreeData;
  Folder, FileName: string;
begin
  if assigned(node) then
  begin
    pData := VST.GetNodeData(Node);
    Folder := pData^.Folder;
    FileName := pData^.FileName;
  end
  else
  begin
    Folder := TagsFolder;
    FileName := rsDefaultFilename;
  end;

  if not InputQuery(rsCaptionTagsFile, rsWriteTagName, FileName) then
    Exit;

  FileName := IncludeTrailingPathDelimiter(Folder) +
    CleanFileName(FileName, True, False) + '.ini';

  if FileExistsUTF8(FileName) then
    Exit;

  // Create and close
  FileClose(FileCreateUTF8(FileName));

  AddFile(FileName, Node);
end;

procedure TfmEmutecaTagTree.AskFolder(Node: PVirtualNode);
var
  pData: PCHXTagTreeData;
  Folder, FolderName: string;
begin
  if assigned(node) then
  begin
    pData := VST.GetNodeData(Node);
    FolderName := pData^.Title;
    Folder := pData^.Folder;
  end
  else
  begin
    Folder := TagsFolder;
    FolderName := rsDefaultFolderName;
  end;

  if not InputQuery(rsCaptionFolderName, rsWriteFolderName, FolderName) then
    Exit;

  FolderName := IncludeTrailingPathDelimiter(Folder) +
    CleanFileName(FolderName, True, False);

  if DirectoryExistsUTF8(FolderName) then
    Exit;

  ForceDirectoriesUTF8(FolderName);

  AddFolder(FolderName, Node);

end;

constructor TfmEmutecaTagTree.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

destructor TfmEmutecaTagTree.Destroy;
begin
  inherited Destroy;
end;

end.
