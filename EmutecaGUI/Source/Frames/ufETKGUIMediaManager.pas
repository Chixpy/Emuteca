unit ufETKGUIMediaManager;
{< TfmETKGUIMediaManager frame unit.

  This file is part of Emuteca GUI.

  Copyright (C) 2006-2019 Chixpy

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 3 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, VirtualTrees, Forms, Controls,
  Graphics, Dialogs, ExtCtrls, ComCtrls, StdCtrls, ActnList,
  LCLType, Buttons, EditBtn, Menus, LazFileUtils, LazUTF8, IniFiles,
  // CHX units
  uCHXStrUtils, uCHXImageUtils, uCHXFileUtils, uCHXDlgUtils,
  // CHX frames
  ufCHXFrame, ufCHXFileListPreview, ufCHXImgListPreview,
  ufCHXTxtListPreview, ufCHXProgressBar,
  // CHX forms
  ufrCHXForm,
  // Emuteca Core units
  uEmutecaConst, uEmutecaRscStr, uEmutecaCommon,
  // Emuteca Core classes
  ucEmuteca, ucEmutecaSystem, ucEmutecaGroup, ucEmutecaSoftware,
  // Emuteca Core frames
  ufEmutecaSystemCBX,
  // Emuteca GUI units
  uETKGUIConst,
  // Emuteca GUI classes
  ucETKGUIConfig;

type
  // Data stored in File Trees
  TFileRow = record
    FileName: string;
    Extension: string;
  end;
  PFileRow = ^TFileRow;

  { TfmETKGUIMediaManager }

  // TODO: Simplify this. Many copypasted code can be functions.

  TfmETKGUIMediaManager = class(TfmCHXFrame)
    actAssignFile: TAction;
    actDeleteFile: TAction;
    actDeleteAllFiles: TAction;
    actRenameGroupTitleWithFilename: TAction;
    actRenameGroupFileWithFilename: TAction;
    actMoveAllFiles: TAction;
    actMoveFile: TAction;
    actRenameGroupTitle: TAction;
    actRenameGroupFile: TAction;
    ActionList: TActionList;
    bRename: TBitBtn;
    chkCopyFile: TCheckBox;
    chkSimilarFiles: TCheckBox;
    eOtherFolder: TDirectoryEdit;
    gbxImages: TGroupBox;
    gbxMusic: TGroupBox;
    gbxOtherFiles: TGroupBox;
    gbxRename: TGroupBox;
    gbxSource: TGroupBox;
    gbxSystem: TGroupBox;
    gbxTarget: TGroupBox;
    gbxTexts: TGroupBox;
    gbxVideos: TGroupBox;
    ilActions: TImageList;
    lbxImages: TListBox;
    lbxMusic: TListBox;
    lbxOtherFiles: TListBox;
    lbxTexts: TListBox;
    lbxVideos: TListBox;
    MenuItem1: TMenuItem;
    MenuItem3: TMenuItem;
    migpRenameGroupFileWithFile: TMenuItem;
    migpRenameGroupTitleWithFile: TMenuItem;
    miflDeleteAllFiles: TMenuItem;
    miflMoveAllFiles: TMenuItem;
    miflMoveFile: TMenuItem;
    MenuItem4: TMenuItem;
    miflDeleteFile: TMenuItem;
    miflAssignFile: TMenuItem;
    migpRenameGroupFile: TMenuItem;
    migpRenameGroupTitle: TMenuItem;
    MenuItem2: TMenuItem;
    migpAssignFile: TMenuItem;
    pCenter: TPanel;
    pcSource: TPageControl;
    pcTarget: TPageControl;
    pImagePreview: TPanel;
    pLeft: TScrollBox;
    pumVSTFiles: TPopupMenu;
    pRight: TPanel;
    pSimilar: TPanel;
    pTextPreview: TPanel;
    pumVSTGroups: TPopupMenu;
    sbSource: TStatusBar;
    sbTarget: TStatusBar;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    pagAllFiles: TTabSheet;
    pagFilesWOGroup: TTabSheet;
    pagFilesWOSoft: TTabSheet;
    pagFilesOtherFolder: TTabSheet;
    pagAllGroups: TTabSheet;
    pagGroupsWOFile: TTabSheet;
    pagAllSoft: TTabSheet;
    pagSoftWOFile: TTabSheet;
    pagOtherFiles: TTabSheet;
    Splitter4: TSplitter;
    tbSimilarThresold: TTrackBar;
    vstFilesAll: TVirtualStringTree;
    vstFilesOtherExt: TVirtualStringTree;
    vstFilesWOGroup: TVirtualStringTree;
    vstFilesWOSoft: TVirtualStringTree;
    vstFilesOtherFolder: TVirtualStringTree;
    vstGroupsAll: TVirtualStringTree;
    vstGroupsWOFile: TVirtualStringTree;
    vstSoftAll: TVirtualStringTree;
    vstSoftWOFile: TVirtualStringTree;
    procedure actAssignFileExecute(Sender: TObject);
    procedure actDeleteAllFilesExecute(Sender: TObject);
    procedure actDeleteFileExecute(Sender: TObject);
    procedure actMoveAllFilesExecute(Sender: TObject);
    procedure actMoveFileExecute(Sender: TObject);
    procedure actRenameGroupFileExecute(Sender: TObject);
    procedure actRenameGroupTitleExecute(Sender: TObject);
    procedure chkSimilarFilesChange(Sender: TObject);
    procedure eOtherFolderAcceptDirectory(Sender: TObject; var Value: string);
    procedure lbxFolderSelectionChange(Sender: TObject; User: boolean);
    procedure pcSourceChange(Sender: TObject);
    procedure pcTargetChange(Sender: TObject);
    procedure tbSimilarThresoldClick(Sender: TObject);
    procedure vstFileCompareNodes(Sender: TBaseVirtualTree;
      Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: integer);
    procedure vstFileKeyDown(Sender: TObject; var Key: word;
      Shift: TShiftState);
    procedure vstFilesGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: integer);
    procedure vstFilesOtherFolderChange(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure vstFilesChange(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure vstNodeClick(Sender: TBaseVirtualTree;
      const HitInfo: THitInfo);
    procedure vstGroupCompareNodes(Sender: TBaseVirtualTree;
      Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: integer);
    procedure vstGroupKeyDown(Sender: TObject; var Key: word;
      Shift: TShiftState);
    procedure vstFileFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstFileGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure vstGroupsChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstGroupsAllInitNode(Sender: TBaseVirtualTree;
      ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure vstKeyPress(Sender: TObject; var Key: char);
    procedure vstSoftChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstSoftCompareNodes(Sender: TBaseVirtualTree;
      Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: integer);
    procedure vstSoftGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure vstGroupGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);

  private
    FCurrGroup: cEmutecaGroup;
    FCurrPreview: TfmCHXFileListPreview;
    FCurrSystem: cEmutecaSystem;
    FEmuteca: cEmuteca;
    FExtFilter: TStrings;
    FfmImagePreview: TfmCHXImgListPreview;
    FfmProgressBar: TfmCHXProgressBar;
    FfmSystemCBX: TfmEmutecaSystemCBX;
    FfmTextPreview: TfmCHXTxtListPreview;
    FGUIConfig: cETKGUIConfig;
    FMediaFiles: TStringList;
    FSHA1Folder: string;
    FSourceFile: string;
    FSourceFolder: string;
    FTargetFile: string;
    FTargetFolder: string;
    procedure SetCurrGroup(AValue: cEmutecaGroup);
    procedure SetCurrPreview(AValue: TfmCHXFileListPreview);
    procedure SetCurrSystem(AValue: cEmutecaSystem);
    procedure SetEmuteca(AValue: cEmuteca);
    procedure SetExtFilter(AValue: TStrings);
    procedure SetGUIConfig(AValue: cETKGUIConfig);
    procedure SetSHA1Folder(AValue: string);
    procedure SetSourceFile(AValue: string);
    procedure SetSourceFolder(AValue: string);
    procedure SetTargetFile(AValue: string);
    procedure SetTargetFolder(AValue: string);

  protected
    // Frames
    // ------
    property fmSystemCBX: TfmEmutecaSystemCBX read FfmSystemCBX;
    property fmImagePreview: TfmCHXImgListPreview read FfmImagePreview;
    property fmTextPreview: TfmCHXTxtListPreview read FfmTextPreview;
    property fmProgressBar: TfmCHXProgressBar read FfmProgressBar;

    // Properties
    // ----------
    property CurrSystem: cEmutecaSystem read FCurrSystem write SetCurrSystem;
    {< Current selected system. }
    property CurrGroup: cEmutecaGroup read FCurrGroup write SetCurrGroup;
    {< Current selected group. }
    property ExtFilter: TStrings read FExtFilter write SetExtFilter;
    {< Extensions of the current selected Media. }
    property CurrPreview: TfmCHXFileListPreview
      read FCurrPreview write SetCurrPreview;
    property MediaFiles: TStringList read FMediaFiles;
    {< Mediafiles assigned to the current game or group. }

    property SourceFile: string read FSourceFile write SetSourceFile;
    {< Name of the source file. }
    property SourceFolder: string read FSourceFolder write SetSourceFolder;
    {< Folder of the source file. }
    property TargetFile: string read FTargetFile write SetTargetFile;
    {< Name of the target file. }
    property TargetFolder: string read FTargetFolder write SetTargetFolder;
    {< Folder of the target file. }

    procedure UpdateStatusBars;
    procedure UpdateVST(aFolder: string);
    {< Update the Virtual String Trees.
      @param(aFolder Folder where search the files and media.)
    }

    function SelectSystem(aSystem: cEmutecaSystem): boolean;
    {< Selects a system. }

    procedure LoadSysFolders;
    {< Loads CurrSystem folders in lbx. }
    procedure LoadSystemSoft;

    // File lists
    // ----------

    function GetCurrentFilesVST: TCustomVirtualStringTree;
    {< Returns the current file list shown.
    }

    procedure FilterFiles;
    {< Show only files with similar name to current selected game or group.
    }

    function AddFileCB(aFolder: string; Info: TSearchRec): boolean;
    {< Adds a file to the lists.
      For use with IterateFolder.
      @param(aFolder Folder where the file is in. @(Not used@))
      @param(Info TSearchRec with file data.)
      @return(Always @true; needed for IterateFolder.)
    }
    function AddFileVSTAllFiles(aName: string): boolean;
    {< Adds a file to the lists.

      @param(aName Name of the file.)
      @return(Always @true @(useless until a reason to stop batch operations
        will be found when adding automatically.@).)
    }
    procedure RemoveFileVSTFiles(aFile: string);
    {< Remove a file from lists (not physically).
      If TargetFolder <> SourceFolder then removed from vstFilesOtherFolder.
      Used for hacky updates.
      @param(aFile Name of the file.)
    }


    // Group / Soft list
    procedure RemoveGroupSoftWOFile(aFile: string);
    {< Remove groups from vstGroupsWOFile and  vstSoftWOFile lists that have
         aFile.
      Used for hacky updates.
      @param(aFile Name of file that maybe is used by groups or soft.)
    }


    // Media
    procedure ChangeGroupMedia(aGroup: cEmutecaGroup);
    {< Change the media preview to the group media.
      @param(aGroup The game group with it's media will be previewed.)
    }
    procedure ChangeSoftMedia(aSoft: cEmutecaSoftware);
    {< Change the media preview to the soft media.
      @param(aGame The game with it's media will be previewed.)
    }
    procedure ChangeFileMedia(aFolder, aFileName: string);
    {< Change the media preview to the file.
      @param(aFolder Folder were the file is.)
      @param(aName Name of the file.)
    }

    function AddFilesOtherFolderCB(aFolder: string;
      Info: TSearchRec): boolean;
    {< Add files or folders to vstFilesOtherFolder.
      @param(aFolder Folder where the file is in. @(not used@))
      @param(Info TSearchRec with folder or file data.)
      @return(Always @true; needed for IterateFolder.)
    }

    procedure UpdateFileOtherFolder(aFolder: string);

    procedure AssignFile;
    {< Renames (or copies) selected file to required filename by selected
      group or soft. }
    procedure DeleteFile;
    {< Deletes current selected (source) file FROM DISC PHYSICALLY. }
    procedure DeleteAllFiles;
    {< Deletes all VISIBLE (i.e. no hidden) files from the current list
         FROM DISC PHYSICALLY. }
    procedure MoveFile;
    {< Moves current selected (source) file to another folder. }
    procedure MoveAllFiles;
    {< Moves all VISIBLE (i.e. no hidden) files from the current list to
        another folder. }

    procedure DoClearFrameData;
    procedure DoLoadFrameData;
    procedure DoLoadGUIIcons(aIconsIni: TIniFile; const aBaseFolder: string);

  public
    property Emuteca: cEmuteca read FEmuteca write SetEmuteca;
    {< Emuteca Core. }
    property SHA1Folder: string read FSHA1Folder write SetSHA1Folder;
    {< Folder SHA1 caché is stored. }
    property GUIConfig: cETKGUIConfig read FGUIConfig write SetGUIConfig;
    {< Config of GUI. }

    class function SimpleForm(aEmuteca: cEmuteca; SelectedSystem: cEmutecaSystem; aGUIIconsIni: string;
      aGUIConfig: cETKGUIConfig): integer;
    {< Creates a form with Media Manager. }

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmETKGUIMediaManager }

procedure TfmETKGUIMediaManager.SetSHA1Folder(AValue: string);
begin
  FSHA1Folder := SetAsFolder(AValue);
end;

procedure TfmETKGUIMediaManager.SetSourceFile(AValue: string);
begin
  FSourceFile := SetAsFile(AValue);

  // We want to update the TargetFile extension.
  TargetFile := FTargetFile;

  // UpdateStatusBars; Updated by SetTargetFile
end;

procedure TfmETKGUIMediaManager.SetSourceFolder(AValue: string);
begin
  FSourceFolder := SetAsFolder(AValue);
  UpdateStatusBars;
end;

procedure TfmETKGUIMediaManager.SetTargetFile(AValue: string);
begin
  FTargetFile := RemoveFromBrackets(ExtractFileNameOnly(AValue));

  if (FTargetFile <> '') and (SourceFile <> '') then
    FTargetFile := FTargetFile + UTF8LowerCase(ExtractFileExt(SourceFile));

  UpdateStatusBars;
end;

procedure TfmETKGUIMediaManager.SetTargetFolder(AValue: string);
begin
  FTargetFolder := SetAsFolder(AValue);
  UpdateStatusBars;
end;

procedure TfmETKGUIMediaManager.DoLoadGUIIcons(aIconsIni: TIniFile;
  const aBaseFolder: string);
begin
  ReadActionsIconsIni(aIconsIni, aBaseFolder, Self.Name,
    ilActions, ActionList);
end;

procedure TfmETKGUIMediaManager.UpdateStatusBars;
begin
  sbSource.Panels[1].Text := SourceFolder + SourceFile;
  sbTarget.Panels[1].Text := TargetFolder + TargetFile;
end;

procedure TfmETKGUIMediaManager.UpdateVST(aFolder: string);
var
  GroupNode, SoftNode, FileNode: PVirtualNode;
  aFileName: TFileRow;
  pFileName: PFileRow;
  aGroup: cEmutecaGroup;
  pGroup: ^cEmutecaGroup;
  aSoft: cEmutecaSoftware;
  pSoft: ^cEmutecaSoftware;
  TmpStr: string;
  FileComp, SkipComp: integer;
begin
  aFolder := SetAsFolder(aFolder);
  if not DirectoryExistsUTF8(aFolder) then
    Exit;

  TargetFolder := aFolder;
  SourceFolder := aFolder;
  SourceFile := '';
  TargetFile := '';

  // Adding all files/folders of the target folder
  vstFilesAll.BeginUpdate;
  vstFilesAll.Clear;
  vstFilesOtherExt.BeginUpdate;
  vstFilesOtherExt.Clear;
  fmProgressBar.UpdTextAndBar('Searching files...',
    'This can take some time', 1, 4, False);
  IterateFolderObj(aFolder, @AddFileCB, False);
  vstFilesAll.EndUpdate;
  vstFilesOtherExt.EndUpdate;

  // vstFilesAll -> vstFilesWOGroup and
  //   vstGroupsAll -> vstGroupsWOFile.
  // ----------------------------------
  fmProgressBar.UpdTextAndBar('Searching...',
    'Files without group and groups without file', 2, 4, False);

  // Sorting vstFilesAll and vstGroupsAll to iterate them;
  vstFilesAll.SortTree(0, VirtualTrees.sdAscending, True); // By filename
  vstGroupsAll.SortTree(1, VirtualTrees.sdAscending, True); // By filename

  vstFilesWOGroup.BeginUpdate;
  vstFilesWOGroup.Clear;
  vstGroupsWOFile.BeginUpdate;
  vstGroupsWOFile.Clear;
  GroupNode := vstGroupsAll.GetFirstChild(nil);
  FileNode := vstFilesAll.GetFirstChild(nil);
  while assigned(GroupNode) or assigned(FileNode) do
  begin
    if not assigned(GroupNode) then
    begin
      pGroup := nil;
      aGroup := nil;
      pFileName := vstFilesAll.GetNodeData(FileNode);
      aFileName := pFileName^;
      FileComp := -1; // Advance File
    end
    else if not assigned(FileNode) then
    begin
      pGroup := vstGroupsAll.GetNodeData(GroupNode);
      aGroup := pGroup^;
      pFileName := nil;
      Finalize(aFileName);
      FileComp := 1;  // Advance Group
    end
    else
    begin
      pFileName := vstFilesAll.GetNodeData(FileNode);
      aFileName := pFileName^;
      pGroup := vstGroupsAll.GetNodeData(GroupNode);
      aGroup := pGroup^;
      FileComp := CompareFilenames(aFileName.FileName, aGroup.MediaFileName);
    end;


    // Adding to vst if they don't match
    // if FileComp = 0 then // Match!! only skip files
    if FileComp < 0 then
    begin // Not match, File is behind Group
      pFileName := vstFilesWOGroup.GetNodeData(
        vstFilesWOGroup.AddChild(nil));
      pFileName^ := aFileName;
    end
    else if FileComp > 0 then // Not match, Group is behind Group File
    begin
      pGroup := vstGroupsWOFile.GetNodeData(
        vstGroupsWOFile.AddChild(nil));
      pGroup^ := aGroup;
    end;

    // Getting next nodes of VST
    // FileComp = 0 executes both
    if FileComp <= 0 then
    begin
      // Next file
      FileNode := vstFilesAll.GetNextSibling(FileNode);

      // Skip repeated file with same name (can be with different extension)
      SkipComp := 0;
      TmpStr := aFileName.FileName; // Old filename
      while Assigned(FileNode) and (SkipComp = 0) do
      begin
        pFileName := vstFilesAll.GetNodeData(FileNode);
        aFileName := pFileName^;
        SkipComp := CompareFilenames(aFileName.FileName, TmpStr);
        if SkipComp = 0 then
          FileNode := vstFilesAll.GetNextSibling(FileNode);
      end;
    end;

    if FileComp >= 0 then
    begin
      // Next group
      GroupNode := vstGroupsAll.GetNextSibling(GroupNode);

      // Skip groups with same media file
      SkipComp := 0;
      TmpStr := aGroup.MediaFileName; // Old MediaFileName
      while Assigned(GroupNode) and (SkipComp = 0) do
      begin
        pGroup := vstGroupsAll.GetNodeData(GroupNode);
        aGroup := pGroup^;
        SkipComp := CompareFilenames(aGroup.MediaFileName, TmpStr);
        if SkipComp = 0 then
          GroupNode := vstGroupsAll.GetNextSibling(GroupNode);
      end;
    end;
  end;
  vstFilesWOGroup.EndUpdate;
  vstGroupsWOFile.EndUpdate;


  // vstFilesWOGroup -> vstFilesWOSoft and
  //   vstSoftAll -> vstSoftWOFile.
  // -------------------------------------
  fmProgressBar.UpdTextAndBar('Searching...',
    'Files without soft and soft without file', 3, 4, False);

  // Sorting vstFilesWOGroup and vstvstAllGroups to iterate them;
  vstFilesWOGroup.SortTree(0, VirtualTrees.sdAscending, True); // By filename
  vstSoftAll.SortTree(2, VirtualTrees.sdAscending, True); // By filename

  vstFilesWOSoft.BeginUpdate;
  vstFilesWOSoft.Clear;
  vstSoftWOFile.BeginUpdate;
  vstSoftWOFile.Clear;
  SoftNode := vstSoftAll.GetFirstChild(nil);
  FileNode := vstFilesWOGroup.GetFirstChild(nil);
  while (assigned(SoftNode)) or (assigned(FileNode)) do
  begin

    // Reading nodes
    if not assigned(SoftNode) then
    begin
      aSoft := nil;
      pSoft := nil;
      pFileName := vstFilesWOGroup.GetNodeData(FileNode);
      aFileName := pFileName^;
      FileComp := -1;
    end
    else if not assigned(FileNode) then
    begin
      pSoft := vstSoftAll.GetNodeData(SoftNode);
      aSoft := pSoft^;
      pFileName := nil;
      Finalize(aFileName);
      FileComp := 1;
    end
    else
    begin
      pFileName := vstFilesWOGroup.GetNodeData(FileNode);
      aFileName := pFileName^;
      pSoft := vstSoftAll.GetNodeData(SoftNode);
      aSoft := pSoft^;
      FileComp := CompareFilenames(aFileName.FileName, aSoft.GetMediaFileName);
    end;

    // Adding to vst if they don't match
    //if FileComp = 0 then // Match!! only skip files
    if FileComp < 0 then
    begin // Not match, File is behind Soft
      pFileName := vstFilesWOSoft.GetNodeData(
        vstFilesWOSoft.AddChild(nil));
      pFileName^ := aFileName;
    end
    else if FileComp > 0 then // Not match, Soft is behind Group File
    begin
      pSoft := vstSoftWOFile.GetNodeData(vstSoftWOFile.AddChild(nil));
      pSoft^ := aSoft;
    end;


    // Getting next nodes of VST
    // FileComp = 0 executes both
    if FileComp <= 0 then
    begin
      // Skip repeated file with same name (can be with different extension)
      TmpStr := aFileName.FileName;

      SkipComp := 0;
      FileNode := vstFilesWOGroup.GetNextSibling(FileNode);
      while Assigned(FileNode) and (SkipComp = 0) do
      begin
        pFileName := vstFilesWOGroup.GetNodeData(FileNode);
        aFileName := pFileName^;
        SkipComp := CompareFilenames(aFileName.FileName, TmpStr);
        if SkipComp = 0 then
          FileNode := vstFilesWOGroup.GetNextSibling(FileNode);
      end;
    end;

    if FileComp >= 0 then
    begin
      // Skip soft with same media file
      TmpStr := aSoft.GetMediaFileName;

      SkipComp := 0;
      SoftNode := vstSoftAll.GetNextSibling(SoftNode);
      while Assigned(SoftNode) and (SkipComp = 0) do
      begin
        pSoft := vstSoftAll.GetNodeData(SoftNode);
        aSoft := pSoft^;
        SkipComp := CompareFilenames(aSoft.GetMediaFileName, TmpStr);
        if SkipComp = 0 then
          SoftNode := vstSoftAll.GetNextSibling(SoftNode);
      end;
    end;
  end;
  vstFilesWOSoft.EndUpdate;
  vstSoftWOFile.EndUpdate;


  fmProgressBar.Finish;
end;

function TfmETKGUIMediaManager.SelectSystem(aSystem: cEmutecaSystem): boolean;
begin
  Result := True;
  CurrSystem := aSystem;
end;

procedure TfmETKGUIMediaManager.FilterFiles;
var
  FileNode: PVirtualNode;
  aTargetFile: string;
  pFileName: PFileRow;
  aVST: TCustomVirtualStringTree;
begin
  aVST := GetCurrentFilesVST;
  if not Assigned(aVST) then
    Exit;

  aTargetFile := Trim(ExtractFileNameOnly(TargetFile));
  aVST.BeginUpdate;
  FileNode := aVST.GetFirstChild(nil);
  while assigned(FileNode) do
  begin
    if chkSimilarFiles.Checked and (aTargetFile <> '') then
    begin
      pFileName := aVST.GetNodeData(FileNode);

      aVST.IsVisible[FileNode] :=
        TextSimilarity(aTargetFile, pFileName^.FileName) >=
        tbSimilarThresold.Position;
    end
    else
    begin
      aVST.IsVisible[FileNode] := True;
    end;

    FileNode := aVST.GetNextSibling(FileNode);
  end;
  aVST.EndUpdate;
end;

function TfmETKGUIMediaManager.AddFileCB(aFolder: string;
  Info: TSearchRec): boolean;
begin
  Result := True;
  if (Info.Attr and faDirectory) <> 0 then
  begin
    if (Info.Name = '.') or (Info.Name = '..') then
      Exit;
    Result := AddFileVSTAllFiles(Info.Name + krsVirtualFolderExt);
  end
  else
    Result := AddFileVSTAllFiles(Info.Name);
end;

function TfmETKGUIMediaManager.AddFileVSTAllFiles(aName: string): boolean;
var
  pFile: PFileRow;
begin
  Result := True;

  if SupportedExtCT(aName, krsVirtualFolderExt) then
  begin
    // It's a folder.
    // TOD0 2: Test if it's empty or not have the current type of mediafiles
    pFile := vstFilesAll.GetNodeData(vstFilesAll.AddChild(nil));

    pFile^.FileName := ExtractFileNameOnly(aName);
    pFile^.Extension := krsVirtualFolderExt;
  end
  else
  begin // Look if it is a compressed archive or current type of mediafiles -
    if SupportedExtSL(aName, ExtFilter) or
      SupportedExtSL(aName, Emuteca.Config.CompressedExtensions) then
      pFile := vstFilesAll.GetNodeData(vstFilesAll.AddChild(nil))
    else
      pFile := vstFilesOtherExt.GetNodeData(vstFilesOtherExt.AddChild(nil));

    pFile^.FileName := ExtractFileNameOnly(aName);
    pFile^.Extension := ExtractFileExt(aName);
  end;
end;

procedure TfmETKGUIMediaManager.RemoveFileVSTFiles(aFile: string);

  procedure RemoveFileFromVST(aVST: TBaseVirtualTree; aFile: string);
  var
    pFileName: PFileRow;
    Nodo: PVirtualNode;
  begin
    aVST.BeginUpdate;

    // From LastChild
    Nodo := aVST.GetLastChild(nil);
    while Assigned(Nodo) do
    begin
      pFileName := aVST.GetNodeData(Nodo);
      if CompareFilenames(pFileName^.FileName + pFileName^.Extension,
        aFile) = 0 then
      begin
        aVST.DeleteNode(Nodo);
        // ...VST is too dynamic, reinit the search
        Nodo := aVST.GetLastChild(nil);
      end
      else
        Nodo := aVST.GetPreviousSibling(Nodo);
    end;
    aVST.EndUpdate;
  end;

begin
  if CompareFilenames(SourceFolder, TargetFolder) = 0 then
  begin
    RemoveFileFromVST(vstFilesAll, aFile);
    RemoveFileFromVST(vstFilesOtherExt, aFile);
    RemoveFileFromVST(vstFilesWOGroup, aFile);
    RemoveFileFromVST(vstFilesWOSoft, aFile);
  end;

  if CompareFilenames(SourceFolder,
    SetAsFolder(eOtherFolder.Directory)) = 0 then
    RemoveFileFromVST(vstFilesOtherFolder, aFile);
end;

procedure TfmETKGUIMediaManager.RemoveGroupSoftWOFile(aFile: string);
var
  Nodo: PVirtualNode;
  PGroup: ^cEmutecaGroup;
  pSoft: ^cEmutecaSoftware;
begin
  aFile := ExtractFileNameOnly(aFile);

  // vstGroupsWOFile
  vstGroupsWOFile.BeginUpdate;
  Nodo := vstGroupsWOFile.GetFirstChild(nil);
  while Assigned(Nodo) do
  begin
    PGroup := vstGroupsWOFile.GetNodeData(Nodo);
    if CompareFilenames(PGroup^.MediaFileName, aFile) = 0 then
    begin
      vstGroupsWOFile.DeleteNode(Nodo);
      // See RemoveFileFromVSTWO comment
      Nodo := vstGroupsWOFile.GetFirstChild(nil);
    end
    else
      Nodo := vstGroupsWOFile.GetNextSibling(Nodo);
  end;
  vstGroupsWOFile.EndUpdate;

  // vstSoftWOFile
  vstSoftWOFile.BeginUpdate;
  Nodo := vstSoftWOFile.GetFirstChild(nil);
  while Assigned(Nodo) do
  begin
    pSoft := vstSoftWOFile.GetNodeData(Nodo);
    if CompareFilenames(pSoft^.GetMediaFileName, aFile) = 0 then
    begin
      vstSoftWOFile.DeleteNode(Nodo);
      // See RemoveFileFromVSTWO comment
      Nodo := vstSoftWOFile.GetFirstChild(nil);
    end
    else
      Nodo := vstSoftWOFile.GetNextSibling(Nodo);
  end;
  vstSoftWOFile.EndUpdate;

end;

procedure TfmETKGUIMediaManager.ChangeGroupMedia(aGroup: cEmutecaGroup);
begin
  if not Assigned(CurrPreview) then
    Exit;
  CurrPreview.FileList := nil;
  MediaFiles.Clear;
  EmuTKSearchAllRelatedFiles(MediaFiles, TargetFolder,
    aGroup.MediaFileName, ExtFilter, True, True, Emuteca.TempFolder);
  CurrPreview.FileList := MediaFiles;
end;

procedure TfmETKGUIMediaManager.ChangeSoftMedia(aSoft: cEmutecaSoftware);
begin
  if not Assigned(CurrPreview) then
    Exit;
  CurrPreview.FileList := nil;
  MediaFiles.Clear;
  EmuTKSearchAllRelatedFiles(MediaFiles, TargetFolder,
    aSoft.GetMediaFileName, ExtFilter, True, True, Emuteca.TempFolder);
  CurrPreview.FileList := MediaFiles;
end;

procedure TfmETKGUIMediaManager.ChangeFileMedia(aFolder, aFileName: string);
begin
  if not Assigned(CurrPreview) then
    Exit;
  CurrPreview.FileList := nil;
  MediaFiles.Clear;

  // TODO: Preview folders and zips

  if SupportedExtSL(aFileName, ExtFilter) then
    MediaFiles.Add(aFolder + aFileName);
  CurrPreview.FileList := MediaFiles;
end;

function TfmETKGUIMediaManager.AddFilesOtherFolderCB(aFolder: string;
  Info: TSearchRec): boolean;
var
  pFile: PFileRow;
begin
  Result := True;

  if (Info.Attr and faDirectory) <> 0 then
  begin
    if (Info.Name = '.') or (Info.Name = '..') then
      Exit;

    // It's a folder
    // TOD0 2: Test if it's empty or not have the current type of mediafiles
    pFile := vstFilesOtherFolder.GetNodeData(
      vstFilesOtherFolder.AddChild(nil));

    pFile^.FileName := Info.Name;
    pFile^.Extension := krsVirtualFolderExt;
  end
  else
  begin
    if not (SupportedExtSL(Info.Name, ExtFilter) or
      SupportedExtSL(Info.Name, Emuteca.Config.CompressedExtensions)) then
      Exit;

    pFile := vstFilesOtherFolder.GetNodeData(
      vstFilesOtherFolder.AddChild(nil));

    pFile^.FileName := ExtractFileNameOnly(Info.Name);
    pFile^.Extension := ExtractFileExt(Info.Name);
  end;
end;

procedure TfmETKGUIMediaManager.UpdateFileOtherFolder(aFolder: string);
begin
  vstFilesOtherFolder.Clear;
  aFolder := SetAsFolder(aFolder);

  if not DirectoryExistsUTF8(aFolder) then
    Exit;

  vstFilesOtherFolder.BeginUpdate;
  fmProgressBar.UpdTextAndBar('Searching files...',
    'This can take some time', 1, 2, False);
  IterateFolderObj(aFolder, @AddFilesOtherFolderCB, False);
  vstFilesOtherFolder.EndUpdate;

  fmProgressBar.Finish;
end;

procedure TfmETKGUIMediaManager.AssignFile;
var
  TargetPath: string;
  SourcePath: string;
  IsFolder: boolean;
  aBool: boolean;
begin
  if (SourceFolder = PathDelim) or (SourceFolder = '') then
    Exit;
  if (TargetFolder = PathDelim) or (TargetFolder = '') then
    Exit;
  if SourceFile = '' then
    Exit;
  if TargetFile = '' then
    Exit;

  TargetPath := TargetFolder + TargetFile;
  SourcePath := SourceFolder + SourceFile;

  // Testing if same file...
  if CompareFilenames(TargetPath, SourcePath) = 0 then
    Exit;

  // Testing if it's a folder
  IsFolder := CompareFileExt(SourceFile, krsVirtualFolderExt) = 0;
  if IsFolder then
  begin // Removing virtual extension of folders
    TargetPath := ExtractFileNameWithoutExt(TargetPath);
    SourcePath := ExtractFileNameWithoutExt(SourcePath);
  end;

  // Checking source existence, may be it's redundant...
  if IsFolder then
    aBool := DirectoryExistsUTF8(SourcePath)
  else
    aBool := FileExistsUTF8(SourcePath);
  if not aBool then
    Exit;

  // Checking target existence
  if IsFolder then
    aBool := DirectoryExistsUTF8(TargetPath)
  else
    aBool := FileExistsUTF8(TargetPath);
  if aBool then
  begin
    if MessageDlg(Format(rsConfirmOverwriteFile, [TargetPath]),
      mtConfirmation, [mbYes, mbNo], -1) = mrNo then
      // TODO 2: Merge folders?
      Exit
    else
    begin
      if IsFolder then
        // HACK: DeleteDirectoryUTF8 don't exists
        DeleteDirectory(UTF8ToSys(TargetPath), False)
      else
        DeleteFileUTF8(TargetPath);
    end;
  end;

  // Copy or rename the file
  if chkCopyFile.Checked then
  begin
    // HACK: Where is CopyFileUTF8 and CopyDirTreeUTF8?
    if not IsFolder then
      CopyFile(UTF8ToSys(SourcePath), UTF8ToSys(TargetPath),
        [cffOverwriteFile, cffCreateDestDirectory], True)
    else
    begin
      CopyDirTree(UTF8ToSys(SourcePath), UTF8ToSys(TargetPath),
        [cffOverwriteFile, cffCreateDestDirectory]);
    end;
  end
  else
  begin
    RenameFileUTF8(SourcePath, TargetPath);
    // Removing Source file from ALL vstFiles
    RemoveFileVSTFiles(SourceFile);
  end;


  // Quick update of VST lists
  // -------------------------

  // Removing Target file from vstGroupWOFile or vstSoftWOFile.
  RemoveGroupSoftWOFile(TargetFile);

  // Adding TargetFile.
  AddFileVSTAllFiles(TargetFile);

  SourceFile := '';

  // Clear TargetFile only if pagGroupsWOFile or vstSoftWOFile is active because
  //  the group is deleted from their list.
  if (GetCurrentFilesVST = vstGroupsWOFile) or
    (GetCurrentFilesVST = vstSoftWOFile) then
  begin
    TargetFile := '';
  end;

  FilterFiles;
end;

procedure TfmETKGUIMediaManager.DeleteFile;
begin
  if (SourceFile = '') or (SourceFolder = '') then
    Exit;

  if MessageDlg(Format(rsCorfirmDeleteFile,
    [SourceFolder + SourceFile]), mtConfirmation, [mbYes, mbNo],
    -1) = mrNo then
    Exit;

  if not DeleteFileUTF8(SourceFolder + SourceFile) then
  begin
    ShowMessageFmt(rsErrorDeletingFile, [SourceFolder + SourceFile]);
    Exit;
  end;

  RemoveFileVSTFiles(SourceFile);
  SourceFile := '';
end;

procedure TfmETKGUIMediaManager.DeleteAllFiles;
var
  aVSTFiles: TCustomVirtualStringTree;
  aNode: PVirtualNode;
  pFile: PFileRow;
  IsFolder, aBool: boolean;
  SourcePath: string;
begin
  aVSTFiles := GetCurrentFilesVST;
  if not Assigned(aVSTFiles) then
    Exit;

  if MessageDlg('Do you want delete ALL visible files?',
    mtConfirmation, [mbYes, mbNo], -1) = mrNo then
    Exit;

  aNode := aVSTFiles.GetFirstChild(nil);
  while Assigned(aNode) do
  begin
    if aVSTFiles.IsVisible[aNode] then
    begin
      pFile := aVSTFiles.GetNodeData(aNode);

      // SourceFolder is in sync with current VSTFile
      // Testing if it's a folder, and removing virtual folder extension.
      IsFolder := CompareFileExt(pFile^.Extension, krsVirtualFolderExt) = 0;
      if IsFolder then
        SourcePath := SourceFolder + pFile^.FileName
      else
        SourcePath := SourceFolder + pFile^.FileName + pFile^.Extension;

      // Checking source existence, may be it's redundant...
      if IsFolder then
        aBool := DirectoryExistsUTF8(SourcePath)
      else
        aBool := FileExistsUTF8(SourcePath);

      if aBool then
      begin
        if not DeleteFileUTF8(SourcePath) then
        begin
          ShowMessageFmt('Error deleting: %0:s', [SourcePath]);
          Exit;
        end;
      end;
    end;
    aNode := aVSTFiles.GetNextSibling(aNode);
  end;

  // Clear vstFilesOtherFolder if needed
  if CompareFilenames(SourceFolder,
    SetAsFolder(eOtherFolder.Directory)) = 0 then
    UpdateFileOtherFolder(SourceFolder);

  // Full update of VSTs
  if CompareFilenames(SourceFolder, TargetFolder) = 0 then
    UpdateVST(TargetFolder);
end;

procedure TfmETKGUIMediaManager.MoveFile;
var
  SourcePath, TargetPath: string;
  IsFolder, aBool: boolean;
begin
  if (SourceFile = '') or (SourceFolder = '') then
    Exit;

  SetDlgInitialDir(SelectDirectoryDialog1, SourceFolder);
  if not SelectDirectoryDialog1.Execute then
    Exit;

  // Testing if it's a folder, and removing virtual folder extension.
  IsFolder := CompareFileExt(SourceFile, krsVirtualFolderExt) = 0;
  if IsFolder then
    SourcePath := SourceFolder + ExtractFileNameOnly(SourceFile)
  else
    SourcePath := SourceFolder + SourceFile;

  // Checking source existence, may be it's redundant...
  if IsFolder then
    aBool := DirectoryExistsUTF8(SourcePath)
  else
    aBool := FileExistsUTF8(SourcePath);
  if not aBool then
    Exit;

  TargetPath := SetAsFolder(SelectDirectoryDialog1.FileName) +
    ExtractFileName(SourcePath);

  // Checking target existence for files
  if IsFolder then
    aBool := DirectoryExistsUTF8(TargetPath)
  else
    aBool := FileExistsUTF8(TargetPath);
  if aBool then
  begin
    if MessageDlg(Format('%0:s already exists. ¿Overwrite?', [TargetPath]),
      mtConfirmation, [mbYes, mbNo], -1) = mrNo then
      // TODO 2: Merge folders?
      Exit
    else
    begin
      if IsFolder then
        DeleteDirectory(UTF8ToSys(TargetPath), False)
      else
        DeleteFileUTF8(TargetPath);
    end;
  end;

  RenameFileUTF8(SourcePath, TargetPath);
  // Removing Source file from ALL vstFiles
  RemoveFileVSTFiles(SourceFile);

  SourceFile := '';
end;

procedure TfmETKGUIMediaManager.MoveAllFiles;
var
  aVSTFiles: TCustomVirtualStringTree;
  aNode: PVirtualNode;
  pFile: pFileRow;
  IsFolder, aBool: boolean;
  SourcePath, TargetPath: string;
begin
  aVSTFiles := GetCurrentFilesVST;

  if not Assigned(aVSTFiles) then
    Exit;

  SetDlgInitialDir(SelectDirectoryDialog1, SourceFolder);
  if not SelectDirectoryDialog1.Execute then
    Exit;

  aNode := aVSTFiles.GetFirstChild(nil);
  while Assigned(aNode) do
  begin
    if aVSTFiles.IsVisible[aNode] then
    begin
      pFile := aVSTFiles.GetNodeData(aNode);

      // SourceFolder is in sync with current VSTFile
      // Testing if it's a folder, and removing virtual folder extension.
      IsFolder := CompareFileExt(pFile^.Extension, krsVirtualFolderExt) = 0;
      if IsFolder then
        SourcePath := SourceFolder + pFile^.FileName
      else
        SourcePath := SourceFolder + pFile^.FileName + pFile^.Extension;

      // Checking source existence, may be it's redundant...
      if IsFolder then
        aBool := DirectoryExistsUTF8(SourcePath)
      else
        aBool := FileExistsUTF8(SourcePath);

      if aBool then
      begin
        TargetPath := SetAsFolder(SelectDirectoryDialog1.FileName) +
          ExtractFileName(SourcePath);

        // Checking target existence for files
        if IsFolder then
          aBool := DirectoryExistsUTF8(TargetPath)
        else
          aBool := FileExistsUTF8(TargetPath);

        if aBool then
        begin
          if MessageDlg(Format('%0:s already exists. ¿Overwrite?',
            [TargetPath]), mtConfirmation, [mbYes, mbNo], -1) = mrYes then
          begin
            if IsFolder then
              DeleteDirectory(UTF8ToSys(TargetPath), False)
            else
              DeleteFileUTF8(TargetPath);

            RenameFileUTF8(SourcePath, TargetPath);
          end;
          // TODO: Merge Folders
        end
        else
          RenameFileUTF8(SourcePath, TargetPath);
      end;
    end;

    aNode := aVSTFiles.GetNextSibling(aNode);
  end;

  // Clear vstFilesOtherFolder if needed
  if CompareFilenames(SourceFolder,
    SetAsFolder(eOtherFolder.Directory)) = 0 then
    UpdateFileOtherFolder(SourceFolder);

  // Full update of VSTs
  if CompareFilenames(SourceFolder, TargetFolder) = 0 then
    UpdateVST(TargetFolder);
end;

procedure TfmETKGUIMediaManager.DoClearFrameData;
begin

end;

procedure TfmETKGUIMediaManager.DoLoadFrameData;
begin
  Enabled := assigned(Emuteca);

  if not Enabled then
  begin
    ClearFrameData;
    Exit;
  end;

end;

procedure TfmETKGUIMediaManager.LoadSysFolders;
begin
  lbxImages.Clear;
  lbxTexts.Clear;
  lbxMusic.Clear;
  lbxVideos.Clear;
  lbxOtherFiles.Clear;

  vstGroupsAll.Clear;
  vstSoftAll.Clear;
  vstGroupsWOFile.Clear;
  vstSoftWOFile.Clear;
  vstFilesAll.Clear;
  vstFilesOtherExt.Clear;
  vstFilesWOGroup.Clear;
  vstFilesWOSoft.Clear;
  // vstFilesOtherFolder.Clear; Not needed

  SourceFile := '';
  SourceFolder := '';
  TargetFile := '';
  TargetFolder := '';

  if not Assigned(CurrSystem) then
    Exit;

  // Loading data if not already loaded
  Emuteca.SystemManager.LoadSystemData(CurrSystem);

  lbxImages.Clear;
  lbxImages.Items.Add('Icons'); // Special images folder
  lbxImages.Items.AddStrings(CurrSystem.ImageCaptions, False);
  lbxTexts.Items.Assign(CurrSystem.TextCaptions);
  lbxMusic.Items.Assign(CurrSystem.MusicCaptions);
  lbxVideos.Items.Assign(CurrSystem.VideoCaptions);

  vstGroupsAll.RootNodeCount := CurrSystem.GroupManager.VisibleList.Count;

  LoadSystemSoft;
end;

procedure TfmETKGUIMediaManager.LoadSystemSoft;
var
  i: integer;
  pSoft: ^cEmutecaSoftware;
  aSoft: cEmutecaSoftware;
begin
  vstSoftAll.Clear;

  if not Assigned(CurrSystem) then
    Exit;

  // Adding only Soft with different filename for its group.
  vstSoftAll.BeginUpdate;
  i := 0;
  while i < CurrSystem.SoftManager.FullList.Count do
  begin
    aSoft := CurrSystem.SoftManager.FullList[i];
    if not aSoft.MatchGroupFile then
    begin
      pSoft := vstSoftAll.GetNodeData(vstSoftAll.AddChild(nil));
      pSoft^ := aSoft;
    end;
    Inc(i);
  end;
  vstSoftAll.EndUpdate;
end;

function TfmETKGUIMediaManager.GetCurrentFilesVST: TCustomVirtualStringTree;
begin

  // TODO: Make this... "dinamic"; search vst in current page...
  case pcSource.ActivePageIndex of
    0: Result := vstFilesWOGroup;
    1: Result := vstFilesWOSoft;
    2: Result := vstFilesAll;
    3: Result := vstFilesOtherExt;
    4: Result := vstFilesOtherFolder;
    else
      Result := nil;
  end;
end;

procedure TfmETKGUIMediaManager.lbxFolderSelectionChange(Sender: TObject;
  User: boolean);
var
  aLBX: TListBox;
begin
  // Don't anything if a lbx is unselected programatically
  if not User then
    Exit;

  aLBX := TListBox(Sender);

  if not assigned(aLBX) then
    Exit;

  // Unselecting other list boxes
  if aLBX <> lbxImages then
    lbxImages.ItemIndex := -1;
  if aLBX <> lbxTexts then
    lbxTexts.ItemIndex := -1;
  if aLBX <> lbxMusic then
    lbxMusic.ItemIndex := -1;
  if aLBX <> lbxVideos then
    lbxVideos.ItemIndex := -1;
  if aLBX <> lbxOtherFiles then
    lbxOtherFiles.ItemIndex := -1;

  ExtFilter := nil;

  if Assigned(CurrPreview) then
    CurrPreview.FileList := nil;
  CurrPreview := nil;

  SourceFile := '';
  SourceFolder := '';

  vstFilesAll.Clear;
  vstFilesOtherExt.Clear;
  vstFilesWOGroup.Clear;
  vstFilesWOSoft.Clear;
  vstGroupsWOFile.Clear;
  vstSoftWOFile.Clear;

  if aLBX.ItemIndex = -1 then
    Exit;

  if aLBX = lbxImages then
  begin
    if Assigned(GUIConfig) then
      ExtFilter := GUIConfig.ImageExtensions;
    CurrPreview := fmImagePreview;
    case aLBX.ItemIndex of
      0: UpdateVST(CurrSystem.IconFolder);
      else
        UpdateVST(CurrSystem.ImageFolders[aLBX.ItemIndex - 1]);
    end;
  end
  else if aLBX = lbxTexts then
  begin
    if Assigned(GUIConfig) then
      ExtFilter := GUIConfig.TextExtensions;
    CurrPreview := fmTextPreview;
    UpdateVST(CurrSystem.TextFolders[aLBX.ItemIndex]);
  end
  else if aLBX = lbxMusic then
  begin
    if Assigned(GUIConfig) then
      ExtFilter := GUIConfig.MusicExtensions;
    UpdateVST(CurrSystem.MusicFolders[aLBX.ItemIndex]);
  end
  else if aLBX = lbxVideos then
  begin
    if Assigned(GUIConfig) then
      ExtFilter := GUIConfig.VideoExtensions;
    UpdateVST(CurrSystem.VideoFolders[aLBX.ItemIndex]);
  end
  else if aLBX = lbxOtherFiles then
  begin
    UpdateVST('');
    // lbxOtherFiles.ItemIndex := -1;
  end
  else
    UpdateVST('');
end;

procedure TfmETKGUIMediaManager.pcSourceChange(Sender: TObject);
begin
  vstFilesAll.ClearSelection;
  vstFilesOtherExt.ClearSelection;
  vstFilesOtherFolder.ClearSelection;
  vstFilesWOGroup.ClearSelection;
  vstFilesWOSoft.ClearSelection;
  SourceFile := '';
end;

procedure TfmETKGUIMediaManager.pcTargetChange(Sender: TObject);
begin
  vstGroupsAll.ClearSelection;
  vstGroupsWOFile.ClearSelection;
  vstSoftAll.ClearSelection;
  vstSoftWOFile.ClearSelection;
  TargetFile := '';
end;

procedure TfmETKGUIMediaManager.tbSimilarThresoldClick(Sender: TObject);
begin
  // In TrackBar, this method means end of changing it.
  if chkSimilarFiles.Checked then
    FilterFiles;
end;

procedure TfmETKGUIMediaManager.vstFileCompareNodes(Sender: TBaseVirtualTree;
  Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: integer);
var
  pFile1, pFile2: PFileRow;
begin
  Result := 0;
  pFile1 := Sender.GetNodeData(Node1);
  pFile2 := Sender.GetNodeData(Node2);

  case Column of
    0: // FileName
      Result := CompareFilenames(pFile1^.FileName, pFile2^.FileName);
    1: // Extension
      Result := CompareFilenames(pFile1^.Extension, pFile2^.Extension);
    else
      ;
  end;
end;

procedure TfmETKGUIMediaManager.vstFileKeyDown(Sender: TObject;
  var Key: word; Shift: TShiftState);
begin
  if Shift = [] then
  begin
    case Key of
      VK_RETURN: actAssignFile.Execute;
      VK_DELETE: actDeleteFile.Execute;
      else
        ;
    end;
  end;
end;

procedure TfmETKGUIMediaManager.vstFilesGetNodeDataSize(
  Sender: TBaseVirtualTree; var NodeDataSize: integer);
begin
  NodeDataSize := SizeOf(TFileRow);
end;

procedure TfmETKGUIMediaManager.vstFilesOtherFolderChange(
  Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  aFileRow: PFileRow;
begin
  SourceFile := '';
  SourceFolder := eOtherFolder.Text;

  if assigned(Sender) and assigned(Node) then
  begin
    aFileRow := Sender.GetNodeData(Node);
    SourceFile := aFileRow^.FileName + aFileRow^.Extension;
  end;

  ChangeFileMedia(SourceFolder, SourceFile);
end;

procedure TfmETKGUIMediaManager.vstFilesChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  aFileRow: PFileRow;
begin
  SourceFile := '';
  SourceFolder := TargetFolder;

  if assigned(Sender) and assigned(Node) then
  begin
    aFileRow := Sender.GetNodeData(Node);
    SourceFile := aFileRow^.FileName + aFileRow^.Extension;
  end;

  ChangeFileMedia(SourceFolder, SourceFile);
end;

procedure TfmETKGUIMediaManager.vstNodeClick(Sender: TBaseVirtualTree;
  const HitInfo: THitInfo);
var
  aTree: TVirtualStringTree;
begin
  // FIX: Clicking on already selected node don't trigger OnChange.
  // Example: Select a File, select a group, when clicked same File again
  //   (or viceversa) Media preview is not updated.
  // On(Node)Click is not called when Keyboard is used, so OnChange must be used.
  // When called On(Node)Click on not selected node OnChange is called too;
  //   so Media Preview is "updated" 2 times ~_~U

  if not (Sender is TVirtualStringTree) then
    Exit;
  aTree := TVirtualStringTree(Sender);

  // We want call OnChange only if same selected node is clicked...
  //   but OnChange is called before... so FocusedNode = HitInfo.HitNode...
  if HitInfo.HitNode <> aTree.FocusedNode then
    Exit;

  if Assigned(aTree.OnChange) then
    aTree.OnChange(aTree, HitInfo.HitNode);
end;

procedure TfmETKGUIMediaManager.vstGroupCompareNodes(Sender: TBaseVirtualTree;
  Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: integer);
var
  pGroup1, pGroup2: ^cEmutecaGroup;
begin
  Result := 0;
  pGroup1 := Sender.GetNodeData(Node1);
  pGroup2 := Sender.GetNodeData(Node2);

  case Column of
    0: Result := UTF8CompareText(pGroup1^.Title, pGroup2^.Title);
    1: Result := CompareFilenames(pGroup1^.MediaFileName,
        pGroup2^.MediaFileName);
    else
      ;
  end;
end;

procedure TfmETKGUIMediaManager.vstGroupKeyDown(Sender: TObject;
  var Key: word; Shift: TShiftState);
begin
  if Shift = [] then
  begin
    case Key of
      VK_RETURN: actAssignFile.Execute;
      else
        ;
    end;
  end;
end;

procedure TfmETKGUIMediaManager.chkSimilarFilesChange(Sender: TObject);
begin
  FilterFiles;
end;

procedure TfmETKGUIMediaManager.eOtherFolderAcceptDirectory(Sender: TObject;
  var Value: string);
begin
  UpdateFileOtherFolder(Value);
end;

procedure TfmETKGUIMediaManager.actAssignFileExecute(Sender: TObject);
begin
  AssignFile;
end;

procedure TfmETKGUIMediaManager.actDeleteAllFilesExecute(Sender: TObject);
begin
  DeleteAllFiles;
end;

procedure TfmETKGUIMediaManager.actDeleteFileExecute(Sender: TObject);
begin
  DeleteFile;
end;

procedure TfmETKGUIMediaManager.actMoveAllFilesExecute(Sender: TObject);
begin
  MoveAllFiles;
end;

procedure TfmETKGUIMediaManager.actMoveFileExecute(Sender: TObject);
begin
  MoveFile;
end;

procedure TfmETKGUIMediaManager.actRenameGroupFileExecute(Sender: TObject);
var
  NewName: string;
begin
  if not Assigned(CurrGroup) then
    Exit;
  NewName := CleanFileName(CurrGroup.SortTitle);

  if not InputQuery(self.Caption, 'Rename group media filename:', NewName) then
    Exit;

  CurrGroup.MediaFileName := NewName;

  TargetFile := CurrGroup.MediaFileName + krsVirtualExt;
  ChangeGroupMedia(CurrGroup);
  FilterFiles;
end;

procedure TfmETKGUIMediaManager.actRenameGroupTitleExecute(Sender: TObject);
var
  NewName: string;
begin
  if not Assigned(CurrGroup) then
    Exit;

  NewName := CurrGroup.Title;

  if not InputQuery(self.Caption, 'Rename group title:', NewName) then
    Exit;

  CurrGroup.Title := NewName;

  if MessageDlg(self.Caption, 'Rename group media filename?',
    mtConfirmation, mbYesNo, '') = mrNo then
    Exit;

  CurrGroup.MediaFileName := CleanFileName(NewName);

  TargetFile := CurrGroup.MediaFileName + krsVirtualExt;
  ChangeGroupMedia(CurrGroup);
  FilterFiles;
end;

procedure TfmETKGUIMediaManager.vstFileFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  pFile: PFileRow;
begin
  pFile := Sender.GetNodeData(Node);
  pFile^.FileName := '';
  pFile^.Extension := '';
  Finalize(pFile^);
  Finalize(pFile);
end;

procedure TfmETKGUIMediaManager.vstFileGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  pFile: PFileRow;
begin
  pFile := Sender.GetNodeData(Node);
  if not Assigned(pFile) then
    Exit;

  case TextType of
    ttNormal: case Column of
        0: CellText := pFile^.FileName;
        1: CellText := pFile^.Extension;
        else
          ;
      end;
    else
      ;
  end;
end;

procedure TfmETKGUIMediaManager.vstGroupsChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  PData: ^cEmutecaGroup;
begin
  TargetFile := '';

  if (not assigned(Sender)) or (not assigned(Node)) then
    Exit;

  PData := Sender.GetNodeData(Node);
  CurrGroup := PData^;

  TargetFile := CurrGroup.MediaFileName + krsVirtualExt;
  ChangeGroupMedia(CurrGroup);
  FilterFiles;
end;

procedure TfmETKGUIMediaManager.vstGroupsAllInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  pGroup: ^cEmutecaGroup;
begin
  if not assigned(CurrSystem) then
    Exit;
  pGroup := Sender.GetNodeData(Node);
  pGroup^ := CurrSystem.GroupManager.VisibleList[Node^.Index];
end;

procedure TfmETKGUIMediaManager.vstKeyPress(Sender: TObject; var Key: char);
begin
  // Removing "Beep" sound when key Return is pressed
  case Key of
    #13, #10: Key := #0;
    else
      ;
  end;
end;

procedure TfmETKGUIMediaManager.vstSoftChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  pSoft: ^cEmutecaSoftware;
begin
  TargetFile := '';

  if (not assigned(Sender)) or (not assigned(Node)) then
    Exit;

  pSoft := Sender.GetNodeData(Node);
  TargetFile := pSoft^.GetMediaFileName + krsVirtualExt;
  CurrGroup := cEmutecaGroup(pSoft^.CachedGroup);

  ChangeSoftMedia(pSoft^);

  FilterFiles;
end;

procedure TfmETKGUIMediaManager.vstSoftCompareNodes(Sender: TBaseVirtualTree;
  Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: integer);
var
  pSoft1, pSoft2: ^cEmutecaSoftware;
begin
  Result := 0;
  pSoft1 := Sender.GetNodeData(Node1);
  pSoft2 := Sender.GetNodeData(Node2);

  case Column of
    0: Result := UTF8CompareText(pSoft1^.Title, pSoft2^.Title);
    1: Result := UTF8CompareText(pSoft1^.CachedGroup.Title,
        pSoft2^.CachedGroup.Title);
    2: Result := CompareFilenames(pSoft1^.GetMediaFileName,
        pSoft2^.GetMediaFileName);
    else
      ;
  end;
end;

procedure TfmETKGUIMediaManager.vstSoftGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  pSoft: ^cEmutecaSoftware;
begin
  pSoft := Sender.GetNodeData(Node);
  if not Assigned(pSoft) then
    Exit;
  if not Assigned(pSoft^) then
    Exit;

  case TextType of
    ttNormal: case Column of
        0: CellText := pSoft^.Title;
        1: CellText := pSoft^.CachedGroup.Title;
        2: CellText := pSoft^.GetMediaFileName;
        else
          ;
      end;
    else
      ;
  end;
end;

procedure TfmETKGUIMediaManager.vstGroupGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  pGroup: ^cEmutecaGroup;
begin
  pGroup := Sender.GetNodeData(Node);
  if not Assigned(pGroup) then
    Exit;
  if not Assigned(pGroup^) then
    Exit;

  case TextType of
    ttNormal: case Column of
        0: CellText := pGroup^.Title;
        1: CellText := pGroup^.MediaFileName;
        else
          ;
      end;
    else
      ;
  end;
end;

procedure TfmETKGUIMediaManager.SetEmuteca(AValue: cEmuteca);
begin
  if FEmuteca = AValue then
    Exit;
  FEmuteca := AValue;

  if Assigned(Emuteca) then
    fmSystemCBX.SystemList := Emuteca.SystemManager.EnabledList
  else
    fmSystemCBX.SystemList := nil;
  fmSystemCBX.SelectedSystem := nil;

  LoadFrameData;
end;

procedure TfmETKGUIMediaManager.SetExtFilter(AValue: TStrings);
begin
  if FExtFilter = AValue then
    Exit;
  FExtFilter := AValue;
end;

procedure TfmETKGUIMediaManager.SetGUIConfig(AValue: cETKGUIConfig);
begin
  if FGUIConfig = AValue then
    Exit;
  FGUIConfig := AValue;

  if assigned(GUIConfig) then
  begin
    // GUIConfigIni := GUIConfig.ConfigFile;
    SHA1Folder := SetAsAbsoluteFile(GUIConfig.GlobalCache, ProgramDirectory);
  end
  else
  begin
    // GUIConfigIni := '';
    SHA1Folder := '';
  end;
end;

procedure TfmETKGUIMediaManager.SetCurrSystem(AValue: cEmutecaSystem);
begin
  if FCurrSystem = AValue then
    Exit;
  FCurrSystem := AValue;

  LoadSysFolders;
end;

procedure TfmETKGUIMediaManager.SetCurrGroup(AValue: cEmutecaGroup);
begin
  if FCurrGroup = AValue then
    Exit;
  FCurrGroup := AValue;
end;

procedure TfmETKGUIMediaManager.SetCurrPreview(AValue: TfmCHXFileListPreview);
begin
  if FCurrPreview = AValue then
    Exit;
  FCurrPreview := AValue;
end;

class function TfmETKGUIMediaManager.SimpleForm(aEmuteca: cEmuteca;
  SelectedSystem: cEmutecaSystem; aGUIIconsIni: string; aGUIConfig: cETKGUIConfig): integer;
var
  aForm: TfrmCHXForm;
  aFrame: TfmETKGUIMediaManager;
begin
  Result := mrNone;

  Application.CreateForm(TfrmCHXForm, aForm);
  try
    aForm.Name := 'frmETKGUIMediaManager';
    aForm.Caption := Format(krsFmtWindowCaption,
      [Application.Title, 'Media Manager']);

    aFrame := TfmETKGUIMediaManager.Create(aForm);
    aFrame.Align := alClient;

    aFrame.GUIConfig := aGUIConfig;
    aFrame.Emuteca := aEmuteca;
    aFrame.fmSystemCBX.SelectedSystem := SelectedSystem;
    // fmSystemCBX.SelectedSystem don't trigger SetSystem() callback.
    aFrame.SelectSystem(SelectedSystem);

    aFrame.Parent := aForm;

    aForm.LoadGUIConfig(aGUIConfig.DefaultFileName);
    aForm.LoadGUIIcons(aGUIIconsIni);
    Result := aForm.ShowModal;
  finally
    aForm.Free;
  end;
end;

constructor TfmETKGUIMediaManager.Create(TheOwner: TComponent);

  procedure CreateFrames;
  begin
    FfmSystemCBX := TfmEmutecaSystemCBX.Create(gbxSystem);
    fmSystemCBX.FirstItem := ETKSysCBXFISelect;
    fmSystemCBX.OnSelectSystem := @SetCurrSystem;
    fmSystemCBX.Align := alTop;
    fmSystemCBX.Parent := gbxSystem;

    FfmImagePreview := TfmCHXImgListPreview.Create(pImagePreview);
    fmImagePreview.Align := alClient;
    fmImagePreview.Parent := pImagePreview;

    FfmTextPreview := TfmCHXTxtListPreview.Create(pTextPreview);
    fmTextPreview.Align := alClient;
    fmTextPreview.Parent := pTextPreview;

    FfmProgressBar := TfmCHXProgressBar.SimpleForm('');
  end;

begin
  inherited Create(TheOwner);

  CreateFrames;

  vstGroupsAll.NodeDataSize := SizeOf(cEmutecaGroup);
  vstGroupsWOFile.NodeDataSize := SizeOf(cEmutecaGroup);
  vstSoftAll.NodeDataSize := SizeOf(cEmutecaSoftware);
  vstSoftWOFile.NodeDataSize := SizeOf(cEmutecaSoftware);
  vstFilesAll.NodeDataSize := SizeOf(TFileRow);
  vstFilesOtherExt.NodeDataSize := SizeOf(TFileRow);
  vstFilesWOGroup.NodeDataSize := SizeOf(TFileRow);
  vstFilesWOSoft.NodeDataSize := SizeOf(TFileRow);
  vstFilesOtherFolder.NodeDataSize := SizeOf(TFileRow);

  pcSource.ActivePageIndex := 0;
  pcTarget.ActivePageIndex := 0;

  FMediaFiles := TStringList.Create;

  OnClearFrameData := @DoClearFrameData;
  OnLoadFrameData := @DoLoadFrameData;
  OnLoadGUIIcons := @DoLoadGUIIcons;
end;

destructor TfmETKGUIMediaManager.Destroy;
begin
  MediaFiles.Free;

  inherited Destroy;
end;

end.
