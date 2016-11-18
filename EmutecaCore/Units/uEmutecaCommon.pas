unit uEmutecaCommon;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  { TODO : Change prefix to krs }
  { TODO : Not all must be global constants... }

  krsEmuteca = 'Emuteca';
  {< Main 'Emuteca' String!!! }

  krsEmutecaGameSubFolder = 'Game/';
  {< Subfolder in temp directory, where games will be decompressed.

    Please attach directory separator}

  { TODO : Next must be unused }

  kGroupSectionKey = 'Group: ';
  {< Key for group sections in database files. }

  // Virtual extensions for internal use.
  // ------------------------------------
  // For dirty tricks...
  kEmutecaVirtualFolderExt = '.(folder)';
  {< Virtual extension used for folders in some contexts. }
  kEmutecaVirtualGroupExt = '.(group)';
  {< Virtual extension used for groups filenames. }
  kEmutecaVirtualGameExt = '.(game)';
  {< Virtual extension used for game filenames. }

  // EXIT CODES for handling some errors
  // Praying for no emulator use these exit codes.
  kEmutecaExecErrorNoGame = -300;
  {< Error code when game is not found. }
  kEmutecaDecompressError = -301;
  {< Error decompressing archive. }

  // Constants for file keys
  krsCRC32 = 'CRC32';
  krsSHA1 = 'SHA1';
  krsFileName = 'FileName';
  krsCustom = 'Custom';

resourcestring

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

  // Formated statistics
  // -------------------
  rsFmtNItems = '%0:d items. (%1:d visible)';
  {< %0:d = Number of items. }

  rsFmtNTimes = '%0:d times.';
  {< %0:d = Number of times. }

  // Default data strings
  // --------------------
  rsNotCached = 'Not Cached'; // Opps...
  rsNever = 'Never';
  rsUnknown = ' Unknown';
  rsFmtApplicationTitle = '%0:s %1:s';
  {<
    %0:s = 'Emuteca' (Application name).
    %1:s = Version.
  }
  rsFmtWindowCaption = '%0:s : %1:s';
  {<
    %0:s = Application.Title (rsFmtApplicationTitle).
    %1:s = Window caption.
  }

type
  TEmutecaProgressCallBack = function(const Title, Info1, Info2: string;
    const Value, MaxValue: int64): boolean of object;
  {< Callback funtion to show progress }

implementation

end.
