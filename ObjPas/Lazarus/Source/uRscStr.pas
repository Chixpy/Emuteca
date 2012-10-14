unit uRscStr;

{$mode objfpc}{$H+}

interface

uses
  uConst;

resourcestring

  // File masks description
  // ----------------------
  rsFileMaskAllFilesDescription = 'All files (' + kFileMaskAllFiles + ')';
  {< Description for 'all files' filemask. }
  rsFileMaskGameDBDescription =
    'Game database file (' + kFileMaskGameDB + ')';
  {< Description for 'games database' filemask. }
  rsFileMaskScriptDescription = 'Script file (' + kFileMaskScript + ')';
  {< Description for 'scripts' filemask. }
  rsFileMaskEmuDBDescription =
    'Emulator database file (' + kFileMaskEmuDB + ')';
  {< Description for 'emulator database' filemask. }
  rsFileMaskSystemDBDescription =
    'System database file (' + kFileMaskSystemDB + ')';
  {< Description for 'system database' filemask. }

  // Actions
  // -------
  rsAddingFile = 'Adding file:';
  {< Action 'Adding file:'. }
  rsUpdatingList = 'Updating List:';
  {< Action 'Updating List:'. }
  rsLoadingGameList = 'Loading games:';
  {< Action 'Loading games:'. }
  rsSavingGameList = 'Saving games:';
  {< Action 'Saving games:'. }
  rsDecompressing = 'Decompressing:';
  {< Action 'Decompressing:'. }
  rsImportingData = 'Importing data:';
  {< Action 'Importing data:'. }
  rsExportingData = 'Exporting data:';
  {< Action 'Exporting data:'. }

  // Game / Group property names
  // ---------------------------
  rsGameKey = 'Key';
  {< Game Key property. }
  rsZones = 'Zones';
  {< Game Zones property. }
  rsDeveloper = 'Developer';
  {< Game Developer property. }
  rsPublisher = 'Publisher';
  {< Game Publisher property. }
  rsVersion = 'Version';
  {< Game Version property. }
  rsFilename = 'Filename';
  {< Game Filename property. }

  // Dialogs
  // -------
  rsAskAssignToGroup = 'Do you want to assign it to the game''s group?';
  {< Asks if a file must be assigned to a games family, instead to a game
       itself. }
  rsChooseImageFileFormat =
    'Do you want to save it in a lossless format?' + LineEnding +
    'YES -> .png (lossless, for screenshots)' + LineEnding +
    'NO -> .jpg (better for photographs or scans)';
  {< Asks which image format must be used when assigning a }
  rsFGMDeleteFile = 'Are you sure that you want to delete this file?' +
    slinebreak + '%0:s';
  {< Asks for deleting a existing file. }


  // Automatic folder creation
  // -------------------------
  rsAutoFolderImg = 'Images/';
  rsAutoFolderImgTitle = 'Title/';
  rsAutoFolderImgInGame = 'In game/';
  rsAutoFolderImgFront = 'Front/';
  rsAutoFolderImgMarquee = 'Spine/';
  rsAutoFolderImgBack = 'Back/';
  rsAutoFolderImgMedia = 'Media/';
  rsAutoFolderImgIcon = 'Icon/';
  rsAutoFolderGames = 'Games/';
  rsAutoFolderMusic = 'Music/';
  rsAutoFolderMusicDemo = 'Demo/';
  rsAutoFolderMusicOST = 'OST/';
  rsAutoFolderMusicMix = 'Mix/';
  rsAutoFolderTemp = 'Temp/';
  rsAutoFolderTxt = 'Texts/';
  rsAutoFolderTxtInfo = 'Information/';
  rsAutoFolderTxtManual = 'Manual/';
  rsAutoFolderTxtCheat = 'Cheat/';
  rsAutoFolderTxtNotes = 'Notes/';
  rsAutoFolderTxtCredits = 'Credits/';
  rsAutoFolderVideo = 'Videos/';
  rsAutoFolderVideoDemo = 'Demo/';
  rsAutoFolderVideoTAS = 'Inputs/';
  rsAutoFolderVideoInGame = 'In game/';
  rsAutoFolderBIOS = 'BIOS/';
  rsAutoFolderSaves = 'Saves/';
  rsAutoFolderTools = 'Tools/';

  { TODO -oChixpy : Unsorted resource strings... :-P }

  rsFGMNGroups = '%0:d groups';
  //< Translatable string:
  rsFGMNGames = '%0:d games';
  //< Translatable string:
  rsFGMNTimes = '%0:d times';
  //< Translatable string:

  rsFGMNever = 'Never';
  //< Translatable string:
  rsFGMUnknown = '!Unknown';
  //< Translatable string:

  rsFGMFolderNotExists = '%0:s' + slinebreak + 'Folder not exists.' +
    slinebreak + 'Do you want create it?';
  //< Translatable string:
  rsFGMConfirmOverwriteFile =
    '%0:s' + slinebreak + 'The file already exists.' + slinebreak +
    'Do you want overwrite it?';
  //< Translatable string:

  rsFGMErrorGameNotFound = 'Game not found:' + slinebreak + '%0:s%1:s';
  //< Translatable string:
  rsFGMErrorEmulator = 'Emulator exited with error code: %0:d';
  //< Translatable string:
  rsFGMPurgeMessage = 'Warning:' + slinebreak +
    'This action will erase all the game and group list.' +
    slinebreak + 'Do you want to continue?';
  //< Translatable string:

  rsFGMSystemRequired = 'You need select a system before do this action.';
  //< Translatable string:

  rsFSMScriptFileSaved = 'Script file saved: %0:s';
  rsFSMEmutecaScript = 'Emuteca Script File';
  rsFSMSaveChanges = 'The source was modified:' + sLineBreak +
    '%0:s' + sLineBreak + 'Do you want to save the changes?';
  rsFSMCurrentSystem = 'Current System: %0:s';
  rsFSMCurrentGroup = 'Current Group: %0:s (%1:s)';
  rsFSMCurrentGame = 'Current Game: %0:s (%1:s)';

  rseNotFilename = 'Not defined filename';
  //< Translatable string: 'Not defined filename'

  rsFABCopyright = 'Copyright';
  rsFABUnderLicense = 'Under license:';

  rsSEECompilationMsg = 'Compilation: %0:s';
  rsSEEExecutionMsg = 'Execution: %0:s';
  rsSEEOK = 'OK';
  rsSEEError = 'Error';

  rsFSMSelectSystem = 'Select a system.';
  rsFSMSystemName = 'System name';
  rsFSMAutoConfigSystem =
    'This action will create many subfolders in:' + sLineBreak +
    '%0:s' + sLineBreak + 'Are you sure?';


  rsEmulatorName = 'Emulator name';
  rsEmulatorIniFilter = 'Emulators Ini File';

  rsfmmSource = 'Source: %0:s';
  //< Translatable string: 'Source: %0:s'
  rsfmmTarget = 'Target: %0:s';
  //< Translatable string: 'Target: %0:s'
  rsfmmDeleteFile = 'Do you want to delete the file?' +
    sLineBreak + sLineBreak + '%0:s';
  //< Translatable string: 'Do you want to delete the file?' + sLineBreak
  //    + sLineBreak + '%0:s'
  rsfmmDeleteAll = 'Do you want to delete all current listed files?' +
    sLineBreak + sLineBreak + 'Folder: %0:s' + sLineBreak +
    'Number of files: %1:d';
  //< Translatable string: 'Do you want to delete all current listed files?'
  //    + sLineBreak + sLineBreak + 'Folder: %0:s' + sLineBreak +
  //    'Number of files: %1:d'
  rsfmmDeleteFileError = 'Error deleting the file:' + sLineBreak + '%0:s';
  //< Translatable string: 'Error deleting the file:' + sLineBreak + '%0:s'

  rsfmmTargetExists =
    'Target file already exists.' + sLineBreak + '%0:s' +
    sLineBreak + 'Do you want to overwrite?';
  //< Translatable string: 'Target file already exists.' + sLineBreak + '%0:s'+
  //    sLineBreak + 'Do you want to overwrite?';

  rsfmmIcons = 'Icons';
  //< Translatable string: 'Icons'
  rsfmmMarquees = 'Spines / Marquees';
  //< Translatable string: 'Spines / Marquees'
  rsfmmDemoMusic = 'Demo music';
  //< Translatable string: 'Demo music'
  rsfmmDemoVideo = 'Demo vídeo';
  //< Translatable string: 'Demo vídeo'

  rsfmmAddingFiles = 'Adding files to the list...';
  //< Translatable string: 'Adding files to the list...'
  rsfmmCopyingFileList = 'Copying file list...';
  //< Translatable string: 'Copying file list...'
  rsfmmSearchFilesWOGroup = 'Searching files without group...';
  //< Translatable string: 'Searching files without group...'
  rsfmmSearchFilesWOGame = 'Searching files without game...';
  //< Translatable string: 'Searching files without game...'

  rsfmmRenameGroup = 'Rename group.';
  rsfmmRenameMediaFileToo =
    'Do you want to rename group''s media filename ' +
    'to match the new name?';
  rsfmmRenameMediaFile = 'Rename group media filename.';

  rsNFiles = '%0:d files found.';
//< Translatable string:


implementation

end.
