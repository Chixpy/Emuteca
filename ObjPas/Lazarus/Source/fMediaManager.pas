{ This file is part of Emuteca

  Copyright (C) 2006-2012 Chixpy

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

{ Unit of rgbMedia Manager form }
unit fMediaManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls, ActnList, EditBtn, Menus, VirtualTrees,
  LCLType, LazUTF8,
  fProgress,
  uConfig, uGameManager, uGame, uGameGroup, uTranslator,
  uCustomUtils, fImageViewer;

const
  CSimilarityThresold = 25;

type

  { TfrmMediaManager }

  TfrmMediaManager = class(TForm)
    actChangeFileName: TAction;
    actDeleteFile: TAction;
    actDeleteAllFiles: TAction;
    actPreviousMedia: TAction;
    actNextMedia: TAction;
    actMoveAllFiles: TAction;
    actMoveFile: TAction;
    actOpenImages: TAction;
    ActionList: TActionList;
    bAssign: TButton;
    bDeleteFile: TButton;
    bDeleteAllFiles: TButton;
    bMoveFile: TButton;
    bMoveAllFiles: TButton;
    chkCopyFile: TCheckBox;
    chkOnlySimilar: TCheckBox;
    eOtherFolder: TDirectoryEdit;
    gbxImages: TGroupBox;
    gbxVideos: TGroupBox;
    gbxOtherFiles: TGroupBox;
    gbxMusic: TGroupBox;
    gbxTexts: TGroupBox;
    ilActions: TImageList;
    lbxTexts: TListBox;
    lbxImages: TListBox;
    lbxMusic: TListBox;
    lbxVideos: TListBox;
    lbxOtherFiles: TListBox;
    miOpenImages: TMenuItem;
    mText: TMemo;
    pagAllGroups: TTabSheet;
    pagFilesWOGroup: TTabSheet;
    pagOtherFiles: TTabSheet;
    pagGroupsWOFile: TTabSheet;
    pagFilesWOGame: TTabSheet;
    pagFilesOtherFolder: TTabSheet;
    pFileOperations: TPanel;
    pInfoMedia: TPanel;
    pmFileList: TPopupMenu;
    pmImage: TPopupMenu;
    pOtherFolder: TPanel;
    pSource: TPanel;
    pTarget: TPanel;
    SelectDirectoryDialog: TSelectDirectoryDialog;
    tbMedia: TToolBar;
    tbPreviousMedia: TToolButton;
    tbNextMedia: TToolButton;
    iImage: TImage;
    lSource: TLabel;
    lTarget: TLabel;
    pcTarget: TPageControl;
    pcSource: TPageControl;
    pMain: TPanel;
    pMedia: TPanel;
    pSystem: TPanel;
    SplitterRight: TSplitter;
    SplitterLeft: TSplitter;
    SplitterTargetSoure: TSplitter;
    Splitter4: TSplitter;
    StatusBar: TStatusBar;
    pagAllGames: TTabSheet;
    pagAllFiles: TTabSheet;
    pLeft: TPanel;
    vstGroupsWOFile: TVirtualStringTree;
    vstFilesWOGroup: TVirtualStringTree;
    vstAllGames: TVirtualStringTree;
    vstAllFiles: TVirtualStringTree;
    vstAllGroups: TVirtualStringTree;
    vstFilesWOGame: TVirtualStringTree;
    vstFilesOtherFolder: TVirtualStringTree;
    vstOtherFiles: TVirtualStringTree;
    procedure actChangeFileNameExecute(Sender: TObject);
    procedure actDeleteAllFilesExecute(Sender: TObject);
    procedure actDeleteFileExecute(Sender: TObject);
    procedure actMoveAllFilesExecute(Sender: TObject);
    procedure actMoveFileExecute(Sender: TObject);
    procedure actNextMediaExecute(Sender: TObject);
    procedure actOpenImagesExecute(Sender: TObject);
    procedure actPreviousMediaExecute(Sender: TObject);
    procedure chkOnlySimilarChange(Sender: TObject);
    procedure eOtherFolderAcceptDirectory(Sender: TObject; var Value: string);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure iImageDblClick(Sender: TObject);
    procedure lbxImagesSelectionChange(Sender: TObject; User: boolean);
    procedure lbxMusicSelectionChange(Sender: TObject; User: boolean);
    procedure lbxOtherSelectionChange(Sender: TObject; User: boolean);
    procedure lbxTextsSelectionChange(Sender: TObject; User: boolean);
    procedure lbxVideosSelectionChange(Sender: TObject; User: boolean);
    procedure vstGroupsKeyPress(Sender: TObject; var Key: char);
    procedure vstGroupsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure vstFilesKeyDown(Sender: TObject; var Key: word;
      Shift: TShiftState);
    procedure vstFilesKeyPress(Sender: TObject; var Key: char);
    procedure vstAllGamesCompareNodes(Sender: TBaseVirtualTree;
      Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: integer);
    procedure vstFilesOtherFolderChange(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure vstGroupsCompareNodes(Sender: TBaseVirtualTree;
      Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: integer);
    procedure vstFilesCompareNodes(Sender: TBaseVirtualTree;
      Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: integer);
    procedure vstFilesChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstFilesFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstFilesGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure vstGamesChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstGamesGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure vstGroupsChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstGroupsGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure vstHeaderClick(Sender: TVTHeader;
      HitInfo: TVTHeaderHitInfo);
  private
    FConfig: cConfig;
    FCurrentMediaIndex: integer;
    FExtFilter: TStrings;
    FGameManager: cGameManager;
    FMediaFiles: TStringList;
    FMultiFile: boolean;
    FrsAddingFiles: String;
    FrsCopyingFileList: String;
    FrsDeleteAll: String;
    FrsDeleteFile: String;
    FrsDeleteFileError: String;
    FrsDemoMusic: String;
    FrsDemoVideo: String;
    FrsIcons: String;
    FrsMarquees: String;
    FrsNFiles: String;
    FrsSearchFilesWOGame: String;
    FrsSearchFilesWOGroup: String;
    FrsSource: String;
    FrsTarget: String;
    FrsTargetExists: String;
    FSourceFile: String;
    FSourceFolder: String;
    FTargetFile: String;
    FTargetFolder: String;
    procedure SetConfig(const AValue: cConfig);
    procedure SetCurrentMediaIndex(const AValue: integer);
    procedure SetExtFilter(const AValue: TStrings);
    procedure SetGameManager(const AValue: cGameManager);
    procedure SetMediaFiles(const AValue: TStringList);
    procedure SetMultiFile(const AValue: boolean);
    procedure SetrsAddingFiles(AValue: String);
    procedure SetrsCopyingFileList(AValue: String);
    procedure SetrsDeleteAll(AValue: String);
    procedure SetrsDeleteFile(AValue: String);
    procedure SetrsDeleteFileError(AValue: String);
    procedure SetrsDemoMusic(AValue: String);
    procedure SetrsDemoVideo(AValue: String);
    procedure SetrsIcons(AValue: String);
    procedure SetrsMarquees(AValue: String);
    procedure SetrsNFiles(AValue: String);
    procedure SetrsSearchFilesWOGame(AValue: String);
    procedure SetrsSearchFilesWOGroup(AValue: String);
    procedure SetrsSource(AValue: String);
    procedure SetrsTarget(AValue: String);
    procedure SetrsTargetExists(AValue: String);
    procedure SetSourceFile(const AValue: String);
    procedure SetSourceFolder(const AValue: String);
    procedure SetTargetFile(const AValue: String);
    procedure SetTargetFolder(const AValue: String);

  protected
    // Strings for translation
    property rsSource: String read FrsSource write SetrsSource;
    //< 'Source:'.
    property rsTarget: String read FrsTarget write SetrsTarget;
    //< 'Target:'.
    property rsDeleteFile: String read FrsDeleteFile write SetrsDeleteFile;
    //< 'Do you want to delete the next file?\n\n%s'.
    property rsDeleteAll: String read FrsDeleteAll write SetrsDeleteAll;
    //< 'Do you want to delete all current listed files?\n\nFolder: %s'.
    property rsDeleteFileError: String
      read FrsDeleteFileError write SetrsDeleteFileError;
    //< 'Error deleting the file:\n\n%s'.
    property rsTargetExists: String
      read FrsTargetExists write SetrsTargetExists;
    //< 'Target file already exists. Do you want to overwrite?'.
    property rsIcons: String read FrsIcons write SetrsIcons;
    //< 'Icons'.
    property rsMarquees: String read FrsMarquees write SetrsMarquees;
    //< 'Side / Marquee'.
    property rsDemoMusic: String read FrsDemoMusic write SetrsDemoMusic;
    //< 'Demo music'
    property rsDemoVideo: String read FrsDemoVideo write SetrsDemoVideo;
    //< 'Demo video'
    property rsAddingFiles: String read FrsAddingFiles write SetrsAddingFiles;
    //< 'Adding files to the list...'
    property rsCopyingFileList: String read FrsCopyingFileList write SetrsCopyingFileList;
    //< 'Copying file list...'
    property rsSearchFilesWOGroup: String read FrsSearchFilesWOGroup write SetrsSearchFilesWOGroup;
    //< 'Searching files without group...'
    property rsSearchFilesWOGame: String read FrsSearchFilesWOGame write SetrsSearchFilesWOGame;
    //< 'Searching files without game...'
    property rsNFiles: String read FrsNFiles write SetrsNFiles;

    property SourceFile: String read FSourceFile write SetSourceFile;
    //< Name of the source file.
    property SourceFolder: String read FSourceFolder write SetSourceFolder;
    {< Folder of the source file.

    Always have the trailing path delimiter
    }
    property TargetFile: String read FTargetFile write SetTargetFile;
    //< Name of the target file.
    property TargetFolder: String read FTargetFolder write SetTargetFolder;
    {< Folder of the target file.

    Always have the trailing path delimiter
    }
    property ExtFilter: TStrings read FExtFilter write SetExtFilter;
    {< Extensions of the current selected rgbMedia.

    One extension for each string without dot.

    This object is not created nor freed, it must be assigned from a existing
    TStringList
    }
    property MultiFile: boolean read FMultiFile write SetMultiFile;
    //< Mode of current selected rgbMedia.
    property MediaFiles: TStringList read FMediaFiles write SetMediaFiles;
    //< Mediafiles assigned to the current game or group.
    property CurrentMediaIndex: integer
      read FCurrentMediaIndex write SetCurrentMediaIndex;
    //< Index of te current media file.

    // TODO 3: Maybe this 4 methods can be reduced to 2 without ofuscate them...
    // TODO 3: AddFile(aFolder, aName: String)
    function AddFile(aFolder: String; Info: TSearchRec): boolean; overload;
    {< Adds a file to the lists in not MultiFile mode.

      For use with IterateFolder.

      @param (aFolder Folder where the file is in.)
      @param (Info TSearchRec with file data.)

      @result (Alwasy @true; needed for IterateFolder.)
    }
    function AddFile(aFolder, aName: String): boolean; overload;
    {< Adds a file to the lists in not MultiFile mode.

      For manual use (and hacky updates).

      @param (aFolder Folder where the file is in.)
      @param (aName Name of the file.)

      @result (Alwasy @true @(useless until a reason to stop batch operations
        will be found.@).)
    }
    function AddFolder(aFolder: String; Info: TSearchRec): boolean;
      overload;
    {< Adds a folder (or compressed archive) to the lists in MultiFile mode.

    For use with IterateFolder.

    @param (aFolder Folder where the file is in.)
    @param (Info TSearchRec with folder or compressed archive data.)

    @result (Always @true; needed for IterateFolder.)
    }
    function AddFolder(aFolder, aName: String): boolean; overload;
    {< Add a folder (or compressed archive) to the lists in MultiFile mode.

      For manual use (and hacky updates)

      @param (aFolder Folder where the file is in.)
      @param (aName Name of the subfolder.)

      @result (Always @true @(useless until a reason to stop batch operations
        will be found@).)
    }

    function AddFilesOtherFolder(aFolder: String;
      Info: TSearchRec): boolean; overload;
    {< Add files or folders to vstFilesOtherFolder.

      @param (aFolder Folder where the file is in.)
      @param (Info TSearchRec with folder or file data.)

      @result (Always @true; needed for IterateFolder.)
    }

    procedure VSTUpdate(aFolder: String);
    {< Update the Virtual String Trees.

      @param (aFolder Folder where search the media.)
    }

    procedure ChangeGroupMedia(aGroup: cGameGroup);
    {< Change the media preview to the group media.

      @param (aGroup The game group with it's media will be previewed.)
    }
    procedure ChangeGameMedia(aGame: cGame);
    {< Change the media preview to the game media.

      @param (aGame The game with it's media will be previewed.)
    }
    procedure ChangeFileMedia(aFolder, aName: String);
    {< Change the media preview to the file.

      @param (aFolder Folder were the file is.)
      @param (aName Name of the file.)
    }
    procedure ClearMedia;
    {< Clear media preview fields.
    }
    procedure ShowMedia;
    {< Update current media preview. }
    procedure OpenImagesInViewer;
    {< Open the current previewed image in Image Viewer. }

    procedure ChangeFileName;
    {< Change the name of the current selected file, to one used by the
      selected game o group.
    }
    procedure DeleteFile;
    {< Delete the current selected (source) file FROM DISC PHYSICALLY. }
    procedure DeleteAllFiles;
    {< Delete all VISIBLE (i.e. no hidden) files from the current list
         FROM DISC PHYSICALLY. }
    procedure MoveFile;
    {< Move current selected (source) file to another folder. }
    procedure MoveAllFiles;
    {< Move all VISIBLE (i.e. no hidden) files from the current list to
        another folder. }

    procedure RemoveFileVSTFiles(aFolder, aFile: String);
    {< Remove a file from lists (not physically).

      Used for hacky updates.

      @param (aFolder Folder were the file is.)
      @param (aFile Name of the file.)
    }
    procedure RemoveGroupWOFile(aFile: String);
    {< Remove groups from vstGroupsWOFile list that uses aFile.

      Used for hacky updates.

      @param (aFile Name of file that maybe is used by groups.)
    }

    function CurrentFileList: TCustomVirtualStringTree;
    procedure ShowSimilarFiles;

  public
    { public declarations }
    property Config: cConfig read FConfig write SetConfig;
    {< Config object. }

    property GameManager: cGameManager read FGameManager write SetGameManager;
    {< GameManager with current system. }
  end;

var
  frmMediaManager: TfrmMediaManager;

implementation

{ TfrmMediaManager }

procedure TfrmMediaManager.FormCreate(Sender: TObject);
begin
  vstAllGroups.NodeDataSize := SizeOf(TObject);
  vstAllGames.NodeDataSize := SizeOf(TObject);
  vstGroupsWOFile.NodeDataSize := SizeOf(TObject);
  vstAllFiles.NodeDataSize := SizeOf(String);
  vstFilesWOGroup.NodeDataSize := SizeOf(String);
  vstFilesWOGame.NodeDataSize := SizeOf(String);
  vstOtherFiles.NodeDataSize := SizeOf(String);
  vstFilesOtherFolder.NodeDataSize := SizeOf(String);

  pcSource.ActivePageIndex := 0;
  pcTarget.ActivePageIndex := 0;

  MediaFiles := TStringList.Create;
end;

procedure TfrmMediaManager.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FMediaFiles);
end;

procedure TfrmMediaManager.iImageDblClick(Sender: TObject);
begin
  OpenImagesInViewer;
end;

procedure TfrmMediaManager.actChangeFileNameExecute(Sender: TObject);
begin
  ChangeFileName;
end;

procedure TfrmMediaManager.actDeleteAllFilesExecute(Sender: TObject);
begin
  DeleteAllFiles;
end;

procedure TfrmMediaManager.actDeleteFileExecute(Sender: TObject);
begin
  DeleteFile;
end;

procedure TfrmMediaManager.actMoveAllFilesExecute(Sender: TObject);
begin
  MoveAllFiles;
end;

procedure TfrmMediaManager.actMoveFileExecute(Sender: TObject);
begin
  MoveFile;
end;

procedure TfrmMediaManager.actNextMediaExecute(Sender: TObject);
begin
  if MediaFiles.Count <= 1 then
    Exit;

  Dec(FCurrentMediaIndex);
  if CurrentMediaIndex < 0 then
    CurrentMediaIndex := MediaFiles.Count - 1;
  ShowMedia;
end;

procedure TfrmMediaManager.actOpenImagesExecute(Sender: TObject);
begin
  OpenImagesInViewer;
end;

procedure TfrmMediaManager.actPreviousMediaExecute(Sender: TObject);
begin
  if MediaFiles.Count <= 1 then
    Exit;

  Inc(FCurrentMediaIndex);
  if CurrentMediaIndex >= MediaFiles.Count then
    CurrentMediaIndex := 0;
  ShowMedia;
end;

procedure TfrmMediaManager.chkOnlySimilarChange(Sender: TObject);
begin
  ShowSimilarFiles;
end;

procedure TfrmMediaManager.eOtherFolderAcceptDirectory(Sender: TObject;
  var Value: string);
begin
  vstFilesOtherFolder.Clear;
  vstFilesOtherFolder.BeginUpdate;
  IterateFolderObj(Value, @AddFilesOtherFolder, False);
  vstFilesOtherFolder.EndUpdate;
  vstFilesOtherFolder.SortTree(0, sdAscending, True);
end;

procedure TfrmMediaManager.lbxImagesSelectionChange(Sender: TObject;
  User: boolean);
var
  aFolder: String;
begin
  if not User then
    Exit;
  if lbxImages.ItemIndex = -1 then
    Exit;

  lbxTexts.ItemIndex := -1;
  lbxMusic.ItemIndex := -1;
  lbxVideos.ItemIndex := -1;
  lbxOtherFiles.ItemIndex := -1;

  ExtFilter := Config.ImageExtensions;
  MultiFile := False;
  case lbxImages.ItemIndex of
    0: aFolder := GameManager.System.IconFolder;
    1: aFolder := GameManager.System.MarqueeFolder;
    else
    begin
      MultiFile := StrToBoolDef(GameManager.System.ImageModes[lbxImages.ItemIndex -
        2], False);
      aFolder := GameManager.System.ImageFolders[lbxImages.ItemIndex - 2];
    end;
  end;
  VSTUpdate(aFolder);
end;

procedure TfrmMediaManager.lbxMusicSelectionChange(Sender: TObject;
  User: boolean);
var
  aFolder: String;
begin
  if not User then
    Exit;
  if lbxMusic.ItemIndex = -1 then
    Exit;

  lbxImages.ItemIndex := -1;
  lbxTexts.ItemIndex := -1;
  lbxVideos.ItemIndex := -1;
  lbxOtherFiles.ItemIndex := -1;

  ExtFilter := Config.MusicExtensions;
  MultiFile := False;
  case lbxMusic.ItemIndex of
    0: aFolder := GameManager.System.DemoMusicFolder;
    else
    begin
      // TODO 3: MultiFile := StrToBoolDef(GameManager.System.MusicModes[lbxMusic.ItemIndex-1], False);
      aFolder := GameManager.System.MusicFolders[lbxMusic.ItemIndex - 1];
    end;
  end;

  VSTUpdate(aFolder);
end;

procedure TfrmMediaManager.lbxOtherSelectionChange(Sender: TObject;
  User: boolean);
begin
  if not User then
    Exit;
  if lbxTexts.ItemIndex = -1 then
    Exit;

  lbxImages.ItemIndex := -1;
  lbxTexts.ItemIndex := -1;
  lbxMusic.ItemIndex := -1;
  lbxVideos.ItemIndex := -1;

  // TODO 2: Change ExtFilter and MultiFile;
  // VSTUpdate(GameManager.System.OtherModes[lbxOther.ItemIndex]);
  // if (MultiFile = False) and (chkExtractZip.Checked) then
  //   ExtractZips(GameManager.System.OtherFolders[lbxOther.ItemIndex]);
end;

procedure TfrmMediaManager.lbxTextsSelectionChange(Sender: TObject;
  User: boolean);
begin
  if not User then
    Exit;
  if lbxTexts.ItemIndex = -1 then
    Exit;

  lbxImages.ItemIndex := -1;
  lbxMusic.ItemIndex := -1;
  lbxVideos.ItemIndex := -1;
  lbxOtherFiles.ItemIndex := -1;

  ExtFilter := Config.TextExtensions;
  MultiFile := StrToBoolDef(
    GameManager.System.TextModes[lbxTexts.ItemIndex], False);
  VSTUpdate(GameManager.System.TextFolders[lbxTexts.ItemIndex]);
end;

procedure TfrmMediaManager.lbxVideosSelectionChange(Sender: TObject;
  User: boolean);
var
  aFolder: String;
begin
  if not User then
    Exit;
  if lbxVideos.ItemIndex = -1 then
    Exit;

  lbxImages.ItemIndex := -1;
  lbxTexts.ItemIndex := -1;
  lbxMusic.ItemIndex := -1;
  lbxOtherFiles.ItemIndex := -1;

  ExtFilter := Config.VideoExtensions;
  MultiFile := False;
  case lbxVideos.ItemIndex of
    0: aFolder := GameManager.System.DemoVideoFolder;
    else
    begin
      // TODO 3: MultiFile := StrToBoolDef(GameManager.System.VideoModes[lbxVideos.ItemIndex-1], False);
      aFolder := GameManager.System.VideoFolders[lbxVideos.ItemIndex - 1];
    end;
  end;

  VSTUpdate(aFolder);
end;

procedure TfrmMediaManager.vstGroupsKeyPress(Sender: TObject; var Key: char);
begin
  case Key of
    #13, #10: Key := #0; // Remove "Beep" when key Return is pressed
  end;
end;

procedure TfrmMediaManager.vstGroupsKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Shift = [] then
  begin
    case Key of
      VK_RETURN: actChangeFileName.Execute;
    end;
  end;
end;

procedure TfrmMediaManager.vstFilesKeyDown(Sender: TObject;
  var Key: word; Shift: TShiftState);
begin
  if Shift = [] then
  begin
    case Key of
      VK_RETURN: actChangeFileName.Execute;
      VK_DELETE: actDeleteFile.Execute;
    end;
  end;
end;

procedure TfrmMediaManager.vstFilesKeyPress(Sender: TObject; var Key: char);
begin
  case Key of
    #13, #10: Key := #0; // Remove "Beep" when key Return is pressed
  end;
end;

procedure TfrmMediaManager.vstAllGamesCompareNodes(Sender: TBaseVirtualTree;
  Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: integer);
var
  Nodo1, Nodo2: ^cGame;
begin
  Result := 0;
  Nodo1 := Sender.GetNodeData(Node1);
  Nodo2 := Sender.GetNodeData(Node2);

  case Column of
    -1, 0: Result := UTF8CompareText(Nodo1^.Name, Nodo2^.Name);
    1: Result := UTF8CompareText(Nodo1^.GameGroup, Nodo2^.GameGroup);
    // TODO 2: LINUX
    2: Result := UTF8CompareText(Nodo1^.FileName, Nodo2^.FileName);
  end;
end;

procedure TfrmMediaManager.vstFilesOtherFolderChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  SourceFile := '';
  if Node = nil then
    Exit;
  SourceFolder := SetAsFolder(eOtherFolder.Text);
  SourceFile := String(Sender.GetNodeData(Node)^);
  ChangeFileMedia(SourceFolder, SourceFile);
end;

procedure TfrmMediaManager.vstGroupsCompareNodes(Sender: TBaseVirtualTree;
  Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: integer);
var
  Nodo1, Nodo2: ^cGameGroup;
begin
  Result := 0;
  Nodo1 := Sender.GetNodeData(Node1);
  Nodo2 := Sender.GetNodeData(Node2);

  case Column of
    -1, 0: Result := UTF8CompareText(Nodo1^.Name, Nodo2^.Name);
    // TODO 1: LINUX
    1: Result := UTF8CompareText(Nodo1^.MediaFileName, Nodo2^.MediaFileName);
  end;
end;

procedure TfrmMediaManager.vstFilesCompareNodes(Sender: TBaseVirtualTree;
  Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: integer);
var
  Nodo1, Nodo2: ^String;
begin
  Result := 0;
  Nodo1 := Sender.GetNodeData(Node1);
  Nodo2 := Sender.GetNodeData(Node2);

  case Column of
    // TODO 1: LINUX
    -1, 0: Result := UTF8CompareText(Nodo1^, Nodo2^);
  end;
end;

procedure TfrmMediaManager.vstFilesChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  SourceFile := '';
  if Node = nil then
    Exit;
  SourceFolder := TargetFolder;
  SourceFile := String(Sender.GetNodeData(Node)^);
  ChangeFileMedia(SourceFolder, SourceFile);
end;

procedure TfrmMediaManager.vstFilesFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  PData: ^String;
begin
  PData := Sender.GetNodeData(Node);
  Finalize(PData^);
end;

procedure TfrmMediaManager.vstFilesGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  PData: ^String;
begin
  PData := Sender.GetNodeData(Node);
  if PData^ = '' then
    Exit;
  case TextType of
    ttNormal: case Column of
        -1, 0: CellText := PData^;
      end;
    ttStatic: ;
  end;
end;

procedure TfrmMediaManager.vstGamesChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  PData: ^cGame;
begin
  TargetFile := '';
  if Node = nil then
    Exit;
  PData := Sender.GetNodeData(Node);
  TargetFile := PData^.FileName;
  ChangeGameMedia(PData^);
end;

procedure TfrmMediaManager.vstGamesGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  PData: ^cGame;
begin
  PData := Sender.GetNodeData(Node);
  if PData^ = nil then
    Exit;
  case TextType of
    ttNormal: case Column of
        -1, 0: CellText := PData^.Name;
        1: CellText := PData^.GameGroup;
        2: CellText := PData^.FileName;
      end;
  end;
end;

procedure TfrmMediaManager.vstGroupsChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  PData: ^cGameGroup;
begin
  TargetFile := '';
  if Node = nil then
    Exit;

  PData := Sender.GetNodeData(Node);
  TargetFile := PData^.MediaFileName;

  ChangeGroupMedia(PData^);
  ShowSimilarFiles;
end;

procedure TfrmMediaManager.vstGroupsGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  PData: ^cGameGroup;
begin
  PData := Sender.GetNodeData(Node);
  if PData^ = nil then
    Exit;
  case TextType of
    ttNormal: case Column of
        -1, 0: CellText := PData^.Name;
        1: CellText := PData^.MediaFileName;
      end;
  end;
end;

procedure TfrmMediaManager.vstHeaderClick(Sender: TVTHeader;
  HitInfo: TVTHeaderHitInfo);
begin
  if Sender.SortColumn <> HitInfo.Column then
    Sender.SortDirection := sdAscending
  else
  begin
    if Sender.SortDirection = sdAscending then
      Sender.SortDirection := sdDescending
    else
      Sender.SortDirection := sdAscending;
  end;

  Sender.SortColumn := HitInfo.Column;

  Sender.Treeview.SortTree(Sender.SortColumn, Sender.SortDirection, True);
end;

procedure TfrmMediaManager.SetConfig(const AValue: cConfig);

  procedure Translate;
  var
    Translator: cTranslator;
  begin
    Translator := cTranslator.Create(Config.LanguageFolder +
      Config.LanguageFile);
    try
      Translator.Section := Self.Name;
      {Self.Caption := }Translator.Translate(Self);

      // Título y paneles
      Self.Caption := Application.Title + ': ' + Self.Caption;

      rsSource := lSource.Caption + ' ';
      rsTarget := lTarget.Caption + ' ';

      rsDeleteFile := Translator.Translate('rsDeleteFile',
        'Do you want to delete the file?\n\n%s');
      rsDeleteAll := Translator.Translate('rsDeleteAll',
        'Do you want to delete all current listed files?\n\nFolder: %s\nNumber of files: %d');
      rsDeleteFileError :=
        Translator.Translate('rsDeleteFileError',
        'Error deleting the file:\n\n%s');

      rsTargetExists := Translator.Translate('rsTargetExists',
        'Target file already exists. Do you want to overwrite?');

      rsIcons := Translator.Translate('rsIcons', 'Icons');
      rsMarquees := Translator.Translate('rsMarquees', 'Spines / Marquees');
      rsDemoMusic := Translator.Translate('rsDemoMusic', 'Demo music');
      rsDemoVideo := Translator.Translate('rsDemoVideo', 'Demo vídeo');

      rsAddingFiles := Translator.Translate(rsAddingFiles, 'Adding files to the list...');
      rsCopyingFileList := Translator.Translate(rsCopyingFileList, 'Copying file list...');
      rsSearchFilesWOGroup := Translator.Translate(rsSearchFilesWOGroup, 'Searching files without group...');
      rsSearchFilesWOGame := Translator.Translate(rsSearchFilesWOGame, 'Searching files without game...');

      rsNFiles := Translator.Translate(rsNFiles, '%d files found.');
    finally
      FreeAndNil(Translator);
    end;
  end;

begin
  FConfig := AValue;
  // Actions icons
  ReadActionsIcons(Config.IconsIniFile, Self.Name, Config.ImagesFolder +
    Config.IconsSubfolder, ilActions, ActionList);

  Translate;
  lSource.Caption := rsSource;
  lTarget.Caption := rsTarget;
end;

procedure TfrmMediaManager.SetCurrentMediaIndex(const AValue: integer);
begin
  if FCurrentMediaIndex = AValue then
    Exit;
  FCurrentMediaIndex := AValue;
end;

procedure TfrmMediaManager.SetExtFilter(const AValue: TStrings);
begin
  if FExtFilter = AValue then
    Exit;
  FExtFilter := AValue;
end;

procedure TfrmMediaManager.SetGameManager(const AValue: cGameManager);
var
  i: integer;
  PData: ^TObject;
begin
  FGameManager := AValue;
  if GameManager = nil then
    Exit;
  if GameManager.System = nil then
    Exit;

  pSystem.Caption := GameManager.System.ID + sLineBreak + '(' +
    GameManager.System.Company + ' ' + GameManager.System.Model + ')';

  lbxImages.Clear;
  lbxImages.Items.Add(rsIcons);
  lbxImages.Items.Add(rsMarquees);
  lbxImages.Items.AddStrings(GameManager.System.ImageCaptions);
  lbxTexts.Clear;
  lbxTexts.Items.AddStrings(GameManager.System.TextCaptions);
  lbxMusic.Clear;
  lbxMusic.Items.AddStrings(GameManager.System.MusicCaptions);
  lbxVideos.Clear;
  lbxVideos.Items.AddStrings(GameManager.System.VideoCaptions);
  lbxOtherFiles.Clear;
  // TODO 2: Add other files

  vstAllGames.Clear;
  vstAllGames.BeginUpdate;
  for i := (GameManager.GameCount - 1) downto 0 do
  begin
    PData := vstAllGames.GetNodeData(vstAllGames.AddChild(nil));
    PData^ := GameManager.GameAtPos(i);
  end;
  vstAllGames.EndUpdate;
  vstAllGames.SortTree(0, sdAscending, True);

  vstAllGroups.Clear;
  vstAllGroups.BeginUpdate;
  for i := (GameManager.GroupCount - 1) downto 0 do
  begin
    PData := vstAllGroups.GetNodeData(vstAllGroups.AddChild(nil));
    PData^ := GameManager.GroupAtPos(i);
  end;
  vstAllGroups.EndUpdate;
  vstAllGames.SortTree(0, sdAscending, True);
end;

procedure TfrmMediaManager.SetMediaFiles(const AValue: TStringList);
begin
  FMediaFiles := AValue;
end;

procedure TfrmMediaManager.SetMultiFile(const AValue: boolean);
begin
  FMultiFile := AValue;
end;

procedure TfrmMediaManager.SetrsAddingFiles(AValue: String);
begin
  FrsAddingFiles := AValue;
end;

procedure TfrmMediaManager.SetrsCopyingFileList(AValue: String);
begin
  FrsCopyingFileList := AValue;
end;

procedure TfrmMediaManager.SetrsDeleteAll(AValue: String);
begin
  FrsDeleteAll := AValue;
end;

procedure TfrmMediaManager.SetrsDeleteFile(AValue: String);
begin
  FrsDeleteFile := AValue;
end;

procedure TfrmMediaManager.SetrsDeleteFileError(AValue: String);
begin
  FrsDeleteFileError := AValue;
end;

procedure TfrmMediaManager.SetrsDemoMusic(AValue: String);
begin
  FrsDemoMusic := AValue;
end;

procedure TfrmMediaManager.SetrsDemoVideo(AValue: String);
begin
  FrsDemoVideo := AValue;
end;

procedure TfrmMediaManager.SetrsIcons(AValue: String);
begin
  FrsIcons := AValue;
end;

procedure TfrmMediaManager.SetrsMarquees(AValue: String);
begin
  FrsMarquees := AValue;
end;

procedure TfrmMediaManager.SetrsNFiles(AValue: String);
begin
  FrsNFiles := AValue;
end;

procedure TfrmMediaManager.SetrsSearchFilesWOGame(AValue: String);
begin
  FrsSearchFilesWOGame := AValue;
end;

procedure TfrmMediaManager.SetrsSearchFilesWOGroup(AValue: String);
begin
  FrsSearchFilesWOGroup := AValue;
end;

procedure TfrmMediaManager.SetrsSource(AValue: String);
begin
  FrsSource := AValue;
end;

procedure TfrmMediaManager.SetrsTarget(AValue: String);
begin
  FrsTarget := AValue;
end;

procedure TfrmMediaManager.SetrsTargetExists(AValue: String);
begin
  FrsTargetExists := AValue;
end;

procedure TfrmMediaManager.SetSourceFile(const AValue: String);
begin
  FSourceFile := AValue;
  // ¿Next instruction is a dummy? Well, not at all
  // We want to update the TargetFile extension and lTarget.Caption
  if TargetFile <> '' then
    TargetFile := FTargetFile;

  lSource.Caption := rsSource + ' ' + SourceFolder + SourceFile;
end;

procedure TfrmMediaManager.SetSourceFolder(const AValue: String);
begin
  FSourceFolder := SetAsFolder(AValue);
  lSource.Caption := rsSource + ' ' + SourceFolder + SourceFile;
end;

procedure TfrmMediaManager.SetTargetFile(const AValue: String);
begin
  FTargetFile := ExtractFileName(RemoveFromBrackets(AValue));

  if SourceFile <> '' then
    FTargetFile := FTargetFile + UTF8LowerCase(ExtractFileExt(SourceFile))
  else
    FTargetFile := FTargetFile + CVirtualGameExt;
  lTarget.Caption := rsTarget + ' ' + TargetFolder + TargetFile;
end;

procedure TfrmMediaManager.SetTargetFolder(const AValue: String);
begin
  FTargetFolder := SetAsFolder(AValue);
  lTarget.Caption := rsTarget + ' ' + TargetFolder + TargetFile;
end;

function TfrmMediaManager.AddFile(aFolder: String;
  Info: TSearchRec): boolean;
begin
  Result := True;
  if (Info.Attr and faDirectory) <> 0 then
    Exit;
  Result := AddFile(aFolder, Info.Name);
end;

function TfrmMediaManager.AddFolder(aFolder: String;
  Info: TSearchRec): boolean;
begin
  Result := True;
  if (Info.Attr and faDirectory) <> 0 then
  begin
    if (Info.Name = '.') or (Info.Name = '..') then
      Exit;
    AddFolder(aFolder, Info.Name + CVirtualFolderExt);
  end
  else
    AddFolder(aFolder, Info.Name);
end;

function TfrmMediaManager.AddFile(aFolder, aName: String): boolean;
var
  PData: ^String;
  Extension: String;
begin
  Result := True;
  Extension := UTF8LowerCase(ExtractFileExt(aName));
  Extension := UTF8Copy(Extension, 2, MAXINT);
  if (ExtFilter.IndexOf(Extension) <> -1) then
    PData := vstAllFiles.GetNodeData(vstAllFiles.AddChild(nil))
  else
    PData := vstOtherFiles.GetNodeData(vstOtherFiles.AddChild(nil));
  // See: TfrmMediaManager.vstAllFilesFreeNode;
  PData^ := aName;
end;

function TfrmMediaManager.AddFolder(aFolder, aName: String): boolean;
var
  Extension: String;
  PData: ^String;
begin
  Result := True;
  Extension := UTF8LowerCase(ExtractFileExt(aName));

  if Extension = CVirtualFolderExt then
  begin
    PData := vstAllFiles.GetNodeData(vstAllFiles.AddChild(nil));
    // See: TfrmMediaManager.vstAllFilesFreeNode;
    PData^ := aName;
  end
  else
  begin // Look if it is a compressed archive
    Extension := UTF8Copy(Extension, 2, MAXINT);
    if (GameManager.CompressedExt.IndexOf(Extension) <> -1) then
      PData := vstAllFiles.GetNodeData(vstAllFiles.AddChild(nil))
    else
      PData := vstOtherFiles.GetNodeData(vstOtherFiles.AddChild(nil));
    // See: TfrmMediaManager.vstAllFilesFreeNode;
    PData^ := aName;
  end;
end;

function TfrmMediaManager.AddFilesOtherFolder(aFolder: String;
  Info: TSearchRec): boolean;
var
  aFileName: String;
  PData: ^String;
begin
  Result := True;

  if (Info.Attr and faDirectory) <> 0 then
  begin
    if (Info.Name = '.') or (Info.Name = '..') then
      Exit;
    aFileName := Info.Name + CVirtualFolderExt;
  end
  else
    aFileName := Info.Name;

  PData := vstFilesOtherFolder.GetNodeData(vstFilesOtherFolder.AddChild(nil));
  // See: TfrmMediaManager.vstAllFilesFreeNode;
  PData^ := aFileName;
end;

procedure TfrmMediaManager.VSTUpdate(aFolder: String);
var
  PStringData: ^String;
  PGameGroup, PGameGroup2: ^cGameGroup;
  PGame: ^cGame;
  aFileName: String;
  Nodo1, Nodo2: PVirtualNode;
  Found, Continue: boolean;
  i,j: Integer;
begin
  if not DirectoryExistsUTF8(aFolder) then
    Exit;

  TargetFolder := aFolder;
  if CurrentFileList = vstOtherFiles then
    SourceFolder := eOtherFolder.Directory
  else
    SourceFolder := aFolder;
  SourceFile := '';
  mText.Clear;
  iImage.Picture.Clear;
  pInfoMedia.Caption := '';

  Application.CreateForm(TfrmProgress, frmProgress);
  try

  // Adding all files/folders of the target folder
  StatusBar.SimpleText := rsAddingFiles;
  Application.ProcessMessages;
  vstAllFiles.Clear;
  vstOtherFiles.Clear;
  vstAllFiles.BeginUpdate;
  vstOtherFiles.BeginUpdate;
  if MultiFile then
  begin // Modo carpetas
    IterateFolderObj(aFolder, @AddFolder, False);
  end
  else
  begin // Modo normal
    IterateFolderObj(aFolder, @AddFile, False);
  end;
  vstAllFiles.EndUpdate;
  vstOtherFiles.EndUpdate;
  vstAllFiles.SortTree(0, sdAscending, True);
  vstOtherFiles.SortTree(0, sdAscending, True);

  // Copying vstAllFiles -> vstFilesWOGroup;
  StatusBar.SimpleText := rsCopyingFileList;
  Application.ProcessMessages;
  vstFilesWOGroup.Clear;
  vstFilesWOGroup.BeginUpdate;
  Nodo1 := vstAllFiles.GetFirstChild(nil);
  while Nodo1 <> nil do
  begin
    PStringData := vstFilesWOGroup.GetNodeData(vstFilesWOGroup.AddChild(nil));
    PStringData^ := String(vstAllFiles.GetNodeData(Nodo1)^);
    Nodo1 := vstAllFiles.GetNextSibling(Nodo1);
  end;
  vstFilesWOGroup.EndUpdate;
  vstFilesWOGroup.SortTree(0, sdAscending, True);

  // Searching for files used by the groups and groups without file
  StatusBar.SimpleText := rsSearchFilesWOGroup;
  Continue := True;
  i := 0;
  j := GameManager.GroupCount;
  Application.ProcessMessages;
  vstFilesWOGroup.BeginUpdate;
  vstGroupsWOFile.BeginUpdate;
  vstGroupsWOFile.Clear;
  Nodo1 := vstAllGroups.GetFirstChild(nil);
  while (Nodo1 <> nil) and (Continue) do
  begin
    PGameGroup := vstAllGroups.GetNodeData(Nodo1);
    if frmProgress <> nil then
      Continue := frmProgress.UpdTextAndBar(rsSearchFilesWOGroup,
        PGameGroup^.Name, '', i, j);

    Nodo2 := vstFilesWOGroup.GetFirstChild(nil);
    Found := False;
    while (Nodo2 <> nil) and (not Found) do
    begin
      PStringData := vstFilesWOGroup.GetNodeData(Nodo2);
      aFileName := ChangeFileExt(PStringData^, CVirtualGroupExt);
      // TODO 1: LINUX
      if UTF8CompareText(aFileName, PGameGroup^.MediaFileName) = 0 then
        Found := True
      else
        Nodo2 := vstFilesWOGroup.GetNextSibling(Nodo2);
    end;

    if Found then
      vstFilesWOGroup.DeleteNode(Nodo2)
    else
    begin
      if not GameManager.GroupMediaExists(aFolder, PGameGroup^,
        ExtFilter, Multifile, True) then
      begin
        PGameGroup2 := vstGroupsWOFile.GetNodeData(
          vstGroupsWOFile.AddChild(nil));
        PGameGroup2^ := PGameGroup^;
      end;
    end;
    Inc(i);
    Nodo1 := vstAllGroups.GetNextSibling(Nodo1);
  end;
  vstGroupsWOFile.EndUpdate;
  vstFilesWOGroup.EndUpdate;
  vstGroupsWOFile.SortTree(0, sdAscending, True);
  vstFilesWOGroup.SortTree(0, sdAscending, True);

  // Searching files not used by games
  StatusBar.SimpleText := rsSearchFilesWOGame;
  Application.ProcessMessages;
  Continue := True;
  i := 0;
  j := vstFilesWOGroup.ChildCount[nil];
  if frmProgress <> nil then
    frmProgress.Show;
  vstFilesWOGame.Clear;
  vstFilesWOGame.BeginUpdate;
  Nodo1 := vstFilesWOGroup.GetFirstChild(nil);
  while (Nodo1 <> nil) and (Continue) do
  begin
    Found := False;
    PStringData := vstFilesWOGroup.GetNodeData(Nodo1);
    aFileName := ExtractFileNameOnly(PStringData^);
    Continue := frmProgress.UpdTextAndBar(rsSearchFilesWOGame,
      aFileName, '', i, j);
    Nodo2 := vstAllGames.GetFirstChild(nil);
    while not Found and (Nodo2 <> nil) do
    begin
      PGame := vstAllGames.GetNodeData(Nodo2);
      if SameFileName(aFileName, RemoveFromBrackets(PGame^.FileName)) then
        Found := True;
      Nodo2 := vstAllGames.GetNextSibling(Nodo2);
    end;

    if not Found then
    begin
      PStringData := vstFilesWOGame.GetNodeData(vstFilesWOGame.AddChild(nil));
      PStringData^ := String(vstFilesWOGroup.GetNodeData(Nodo1)^);
    end;

    Nodo1 := vstFilesWOGroup.GetNextSibling(Nodo1);
    Inc(i);
  end;
  vstFilesWOGame.EndUpdate;
  vstFilesWOGame.SortTree(0, sdAscending, True);

  finally
    FreeAndNil(frmProgress);
  end;

  StatusBar.SimpleText := '';
end;

procedure TfrmMediaManager.ChangeGroupMedia(aGroup: cGameGroup);
begin
  ClearMedia;
  MediaFiles.Clear;
  CurrentMediaIndex := -1;

  GameManager.SearchGroupMedia(MediaFiles, TargetFolder, aGroup,
    ExtFilter, MultiFile, True);

  if MediaFiles.Count < 1 then
    Exit;

  if MediaFiles.Count > 2 then
    StatusBar.SimpleText := Format(rsNFiles, [MediaFiles.Count])
  else
    StatusBar.SimpleText := MediaFiles.CommaText;
  CurrentMediaIndex := 0;
  ShowMedia;
end;

procedure TfrmMediaManager.ChangeGameMedia(aGame: cGame);
begin
  ClearMedia;
  MediaFiles.Clear;
  CurrentMediaIndex := -1;
  GameManager.SearchGameMedia(MediaFiles, TargetFolder, aGame,
    ExtFilter, MultiFile, True);
  if MediaFiles.Count < 1 then
    Exit;

  StatusBar.SimpleText := MediaFiles.CommaText;
  CurrentMediaIndex := 0;
  ShowMedia;
end;

procedure TfrmMediaManager.ChangeFileMedia(aFolder, aName: String);
var
  aExt: String;
begin
  ClearMedia;
  MediaFiles.Clear;
  CurrentMediaIndex := -1;

  aExt := UTF8LowerCase(UTF8Copy(ExtractFileExt(aName), 2, MAXINT));
  if GameManager.CompressedExt.IndexOf(aExt) = -1 then
    MediaFiles.Add(aFolder + aName)
  else
    // TODO 2: ¿Extraer todos los fichero del zip?
  ;
  if MediaFiles.Count < 1 then
    Exit;

  StatusBar.SimpleText := MediaFiles.CommaText;
  CurrentMediaIndex := 0;
  ShowMedia;
end;

procedure TfrmMediaManager.ClearMedia;
begin
  iImage.Picture.Clear;
  mText.Clear;
end;

procedure TfrmMediaManager.ShowMedia;
var
  aMediaFile: String;
  aExt: String;
begin
  ClearMedia;
  if not (CurrentMediaIndex in [0..MediaFiles.Count - 1]) then
    Exit;

  aMediaFile := MediaFiles[CurrentMediaIndex];
  aExt := UTF8LowerCase(UTF8Copy(ExtractFileExt(aMediaFile), 2, MAXINT));

  if Config.ImageExtensions.IndexOf(aExt) <> -1 then
  begin
    iImage.Picture.LoadFromFile(aMediaFile);
    pInfoMedia.Caption := IntToStr(iImage.Picture.Width) + 'x' +
      IntToStr(iImage.Picture.Height);
  end;

  if Config.TextExtensions.IndexOf(aExt) <> -1 then
  begin
    mText.Lines.LoadFromFile(UTF8ToSys(aMediaFile));
    pInfoMedia.Caption := IntToStr(mText.Lines.Count);
  end;
end;

procedure TfrmMediaManager.OpenImagesInViewer;
var
  FormIV: TfrmImageViewer;
begin
  if (MediaFiles.Count = 0) or (iImage.Picture = nil) then
    Exit;
  Application.CreateForm(TfrmImageViewer, FormIV);
  try
    FormIV.Config := Self.Config;
    FormIV.AddImages(MediaFiles, CurrentMediaIndex);
    FormIV.ShowModal;
  finally
    FreeAndNil(FormIV);
  end;
end;

procedure TfrmMediaManager.ChangeFileName;
var
  TargetPath: String;
  SourcePath: String;
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

  if TargetPath = SourcePath then
    Exit;

  // TODO 1: LINUX
  IsFolder := UTF8CompareText(ExtractFileExt(SourceFile), CVirtualFolderExt) = 0;

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

  // Checking target existence for files
  if IsFolder then
    aBool := DirectoryExistsUTF8(TargetPath)
  else
    aBool := FileExistsUTF8(TargetPath);
  if aBool then
  begin
    if MessageDlg(Format(rsTargetExists, [TargetPath]),
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

  // Copy or rename the file
  if chkCopyFile.Checked then
  begin
    // TODO 5: HACK: Where is CopyFileUTF8?
    // TODO 4: How can I copy folders?
    if not IsFolder then
      CopyFile(UTF8ToSys(SourcePath), UTF8ToSys(TargetPath))
    else
    begin
      ShowMessage('By now, I can''t copy folders.' + sLineBreak +
        'Uncheck "Don''t delete source file" checkbox.');
    end;
  end
  else
  begin
    RenameFileUTF8(SourcePath, TargetPath);
    // Removing Source file from ALL vstFiles
    RemoveFileVSTFiles(SourceFolder, SourceFile);
  end;

  // Quick update of VST lists
  // -------------------------
  // Removing Target file from vstGroupWOFile.
  RemoveGroupWOFile(TargetFile);

  // Adding TargetFile.
  if MultiFile then
    AddFolder(TargetFolder, TargetFile)
  else
    AddFile(TargetFolder, TargetFile);

  SourceFile := '';

  // Clear TargetFile only if pagGroupsWOFile is active because
  //  the group is deleted from its list.
  if pcTarget.ActivePage = pagGroupsWOFile then
  begin
    TargetFile := '';
    ClearMedia;
  end;

  ShowSimilarFiles;
end;

procedure TfrmMediaManager.DeleteFile;
begin
  if SourceFile = '' then
    Exit;
  if SourceFolder = '' then
    Exit;

  if MessageDlg(Format(rsDeleteFile, [SourceFolder + SourceFile]),
    mtConfirmation, [mbYes, mbNo], -1) = mrNo then
    Exit;

  if not DeleteFileUTF8(SourceFolder + SourceFile) then
  begin
    ShowMessageFmt(rsDeleteFileError, [SourceFolder + SourceFile]);
    Exit;
  end;

  RemoveFileVSTFiles(SourceFolder, SourceFile);
  SourceFile := '';

  ClearMedia;
end;

procedure TfrmMediaManager.DeleteAllFiles;
var
  aFolder: String;
  PStringData: ^String;
  Nodo: PVirtualNode;
  aVST: TBaseVirtualTree;
begin
  aVST := CurrentFileList;
  if aVST = nil then Exit;

  if aVST = vstFilesOtherFolder then
    aFolder := SetAsFolder(eOtherFolder.Directory)
  else
    aFolder := SourceFolder;
  if aFolder = '' then Exit;

  if MessageDlg(Format(rsDeleteAll, [SourceFolder, aVST.ChildCount[nil]]),
    mtConfirmation, [mbYes, mbNo], -1) = mrNo then
    Exit;

  aVST.BeginUpdate;
  Nodo := aVST.GetFirstChild(nil);
  while Nodo<>nil do
  begin
    if not aVST.IsHidden[Nodo] then
    begin // Only delete it if not hidden
      PStringData := aVST.GetNodeData(Nodo);
      DeleteFileUTF8(aFolder + PStringData^);

      RemoveFileVSTFiles(aFolder, PStringData^);

      Nodo := aVST.GetFirstChild(nil);
    end
    else
    begin
      Nodo := aVST.GetNextSibling(Nodo);
    end;
  end;
  aVST.EndUpdate;
end;

procedure TfrmMediaManager.MoveFile;
begin
  // TODO 1: Mover fichero
  if (SourceFolder = '') or (SourceFile = '') then Exit;
  if not SelectDirectoryDialog.Execute then Exit;
  SelectDirectoryDialog.FileName := SetAsFolder(SelectDirectoryDialog.FileName);

  if FileExistsUTF8(SelectDirectoryDialog.FileName + SourceFile) then
  begin
    // TODO 2 -oChixpy : Error message... file already exists
    Exit;
  end;

  if not RenameFileUTF8(SourceFolder + SourceFile,
          SelectDirectoryDialog.FileName + SourceFile) then
  begin
    // TODO 2 -oChixpy : Error message... Can't be moved
    Exit;
  end;

  RemoveFileVSTFiles(SelectDirectoryDialog.FileName, SourceFile);
end;

procedure TfrmMediaManager.MoveAllFiles;
var
  aFolder: String;
  PStringData: ^String;
  Nodo: PVirtualNode;
  aVST: TBaseVirtualTree;
begin
  aVST := CurrentFileList;
  if aVST = nil then Exit;

  if aVST = vstFilesOtherFolder then
    aFolder := SetAsFolder(eOtherFolder.Directory)
  else
    aFolder := SourceFolder;

  if aFolder = '' then Exit;
  if not SelectDirectoryDialog.Execute then Exit;
  SelectDirectoryDialog.FileName := SetAsFolder(SelectDirectoryDialog.FileName);

  aVST.BeginUpdate;
  Nodo := aVST.GetFirstChild(nil);
  while Nodo<>nil do
  begin
    if not aVST.IsHidden[Nodo] then
    begin // Only move it if not hidden
      PStringData := aVST.GetNodeData(Nodo);

      if not FileExistsUTF8(SelectDirectoryDialog.FileName + PStringData^) then
      begin
        if RenameFileUTF8(aFolder + PStringData^,
          SelectDirectoryDialog.FileName + PStringData^) then
        begin
          // Well, actually we are not trully updating the lists...
          RemoveFileVSTFiles(aFolder, PStringData^);

          Nodo := aVST.GetFirstChild(nil);
        end
        else
        begin
          Nodo := aVST.GetNextSibling(Nodo); // Cant move file...
        end;
      end
      else
      begin
        Nodo := aVST.GetNextSibling(Nodo); // File already exists...
      end;
    end
    else
    begin
      Nodo := aVST.GetNextSibling(Nodo); // Hidden node...
    end;
  end;
  aVST.EndUpdate;
end;

procedure TfrmMediaManager.RemoveFileVSTFiles(aFolder, aFile: String);

  procedure RemoveFileFromVST(aVST: TBaseVirtualTree; aFile: String);
  var
    PStringData: ^String;
    Nodo: PVirtualNode;
  begin
    aVST.BeginUpdate;

    // From LastChild, why? No body cares, may be it's better than begin
    //   from the begining.
    Nodo := aVST.GetLastChild(nil);
    while (Nodo <> nil) do
    begin
      PStringData := aVST.GetNodeData(Nodo);
      if PStringData^ = aFile then
      begin
        aVST.DeleteNode(Nodo);
        // VST is too dynamic...
        Nodo := aVST.GetLastChild(nil);
      end
      else
        Nodo := aVST.GetPreviousSibling(Nodo);
    end;
    aVST.EndUpdate;
  end;

begin
  aFolder := SetAsFolder(aFolder);
  if aFolder = TargetFolder then
  begin
    RemoveFileFromVST(vstAllFiles, aFile);
    RemoveFileFromVST(vstOtherFiles, aFile);
    RemoveFileFromVST(vstFilesWOGroup, aFile);
    RemoveFileFromVST(vstFilesWOGame, aFile);
  end;

  // TODO 1: LINUX
  if UTF8CompareText(aFolder, SetAsFolder(eOtherFolder.Directory)) = 0 then
    RemoveFileFromVST(vstFilesOtherFolder, aFile);
end;

procedure TfrmMediaManager.RemoveGroupWOFile(aFile: String);
var
  Nodo: PVirtualNode;
  PGroup: ^cGameGroup;
begin
  // Meh, I don't like this way, but...
  aFile := ExtractFileNameOnly(aFile) + CVirtualGroupExt;
  vstGroupsWOFile.BeginUpdate;
  Nodo := vstGroupsWOFile.GetFirstChild(nil);
  while (Nodo <> nil) do
  begin
    PGroup := vstGroupsWOFile.GetNodeData(Nodo);
    // TODO: LINUX
    if UTF8CompareText(PGroup^.MediaFileName, aFile) = 0 then
    begin
      vstGroupsWOFile.DeleteNode(Nodo);
      // See RemoveFileFromVST comment
      Nodo := vstGroupsWOFile.GetFirstChild(nil);
    end
    else
      Nodo := vstGroupsWOFile.GetNextSibling(Nodo);
  end;
  vstGroupsWOFile.EndUpdate;
end;

function TfrmMediaManager.CurrentFileList: TCustomVirtualStringTree;
begin
  case pcSource.ActivePageIndex of
    0: Result := vstAllFiles;
    1: Result := vstOtherFiles;
    2: Result := vstFilesWOGroup;
    3: Result := vstFilesWOGame;
    4: Result := vstFilesOtherFolder;
  else
    Result := nil;
  end;
end;

procedure TfrmMediaManager.ShowSimilarFiles;
var
  Nodo: PVirtualNode;
  aTargetFile: String;
  PStringData: ^String;
  aVST: TCustomVirtualStringTree;
begin
  aVST:=CurrentFileList;
  if aVST = nil then Exit;

  aTargetFile := Trim(ExtractFileNameOnly(TargetFile));
  aVST.BeginUpdate;
  Nodo := aVST.GetFirstChild(nil);
  while Nodo <> nil do
  begin
    if chkOnlySimilar.Checked and (aTargetFile <> '') then
    begin
      PStringData := aVST.GetNodeData(Nodo);
      aVST.IsVisible[Nodo] := TextSimilarity(aTargetFile, PStringData^) >=
        CSimilarityThresold;
    end
    else
    begin
      aVST.IsVisible[Nodo] := True;
    end;

    Nodo := aVST.GetNextSibling(Nodo);
  end;
  aVST.EndUpdate;
end;

initialization
  {$I fMediaManager.lrs}

end.

