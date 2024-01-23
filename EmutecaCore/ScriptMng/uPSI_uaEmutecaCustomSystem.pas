unit uPSI_uaEmutecaCustomSystem;

{< caEmutecaCustomSystem export for Pascal Script.

  This file is part of Emuteca Core.

  Copyright (C) 2018-2023 Chixpy
}
interface

uses
  SysUtils, Classes, uPSComponent, uPSRuntime, uPSCompiler,
  // Emuteca Core units
  uEmutecaConst,
  // Emuteca Core classes
  ucEmutecaPlayingStats, uaEmutecaCustomSystem;

type

  TPSImport_uaEmutecaCustomSystem = class(TPSPlugin)
    procedure CompileImport1(CompExec: TPSScript); override;
    procedure ExecImport1(CompExec: TPSScript;
      const ri: TPSRuntimeClassImporter); override;
  end;

procedure SIRegister_caEmutecaCustomSystem(CL: TPSPascalCompiler);
procedure SIRegister_uaEmutecaCustomSystem(CL: TPSPascalCompiler);

procedure RIRegister_caEmutecaCustomSystem(CL: TPSRuntimeClassImporter);
procedure RIRegister_uaEmutecaCustomSystem(CL: TPSRuntimeClassImporter);

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Pascal Script', [TPSImport_uaEmutecaCustomSystem]);
end;

procedure SIRegister_caEmutecaCustomSystem(CL: TPSPascalCompiler);
begin
  //with RegClassS(CL,'caCHXStorableIni', 'caEmutecaCustomSystem') do
  with CL.AddClassN(CL.FindClass('caCHXStorableIni'),
      'caEmutecaCustomSystem') do
  begin
    RegisterProperty('TempFolder', 'string', iptrw);

    RegisterProperty('ID', 'string', iptrw);
    RegisterProperty('Title', 'string', iptrw);
    RegisterProperty('ListFileName', 'string', iptrw);
    RegisterProperty('Enabled', 'boolean', iptrw);
    RegisterProperty('ExtractAll', 'boolean', iptrw);
    RegisterProperty('MergeableGroups', 'boolean', iptrw);
    RegisterProperty('BaseFolder', 'string', iptrw);
    RegisterProperty('WorkingFolder', 'string', iptrw);

    RegisterProperty('MainEmulator', 'string', iptrw);
    RegisterProperty('OtherEmulators', 'TStringList', iptr);
    RegisterProperty('CoreIDs', 'TStringList', iptr);

    RegisterProperty('IconFile', 'string', iptrw);
    RegisterProperty('ImageFile', 'string', iptrw);
    RegisterProperty('SoftIconFile', 'string', iptrw);

    RegisterProperty('IconFolder', 'string', iptrw);
    RegisterProperty('LogoFolder', 'string', iptrw);
    RegisterProperty('ImageFolders', 'TStringList', iptr);
    RegisterProperty('ImageCaptions', 'TStringList', iptr);

    RegisterProperty('InfoText', 'string', iptrw);
    RegisterProperty('TextFolders', 'TStringList', iptr);
    RegisterProperty('TextCaptions', 'TStringList', iptr);

    RegisterProperty('MusicFolders', 'TStringList', iptr);
    RegisterProperty('MusicCaptions', 'TStringList', iptr);

    RegisterProperty('VideoFolders', 'TStringList', iptr);
    RegisterProperty('VideoCaptions', 'TStringList', iptr);

    RegisterProperty('OtherFolders', 'TStringList', iptr);
    RegisterProperty('OtherFExt', 'TStringList', iptr);
    RegisterProperty('OtherFCapt', 'TStringList', iptr);

    RegisterProperty('SoftExportKey', 'TEmutecaSoftExportKey', iptrw);
    RegisterProperty('Extensions', 'TStringList', iptr);
    RegisterProperty('Stats', 'cEmutecaPlayingStats', iptr);

    RegisterMethod('function CompareID(aID: string): integer;');
    RegisterMethod('function MatchID(aID: string): boolean;');
  end;
end;

procedure SIRegister_uaEmutecaCustomSystem(CL: TPSPascalCompiler);
begin
  SIRegister_caEmutecaCustomSystem(CL);
end;

procedure caEmutecaCustomSystemTempFolder_R(Self: caEmutecaCustomSystem;
  var T: string);
begin
  T := Self.TempFolder;
end;

procedure caEmutecaCustomSystemTempFolder_W(Self: caEmutecaCustomSystem;
  const T: string);
begin
  Self.TempFolder := T;
end;

procedure caEmutecaCustomSystemID_R(Self: caEmutecaCustomSystem;
  var T: string);
begin
  T := Self.ID;
end;

procedure caEmutecaCustomSystemID_W(Self: caEmutecaCustomSystem;
  const T: string);
begin
  Self.ID := T;
end;

procedure caEmutecaCustomSystemTitle_R(Self: caEmutecaCustomSystem;
  var T: string);
begin
  T := Self.Title;
end;

procedure caEmutecaCustomSystemTitle_W(Self: caEmutecaCustomSystem;
  const T: string);
begin
  Self.Title := T;
end;

procedure caEmutecaCustomSystemListFileName_R(Self: caEmutecaCustomSystem;
  var T: string);
begin
  T := Self.ListFileName;
end;

procedure caEmutecaCustomSystemListFileName_W(Self: caEmutecaCustomSystem;
  const T: string);
begin
  Self.ListFileName := T;
end;

procedure caEmutecaCustomSystemEnabled_R(Self: caEmutecaCustomSystem;
  var T: boolean);
begin
  T := Self.Enabled;
end;

procedure caEmutecaCustomSystemEnabled_W(Self: caEmutecaCustomSystem;
  const T: boolean);
begin
  Self.Enabled := T;
end;

procedure caEmutecaCustomSystemBaseMergeableGroups_R(Self: caEmutecaCustomSystem;
  var T: boolean);
begin
  T := Self.MergeableGroups;
end;

procedure caEmutecaCustomSystemBaseMergeableGroups_W(Self: caEmutecaCustomSystem;
  const T: boolean);
begin
  Self.MergeableGroups := T;
end;

procedure caEmutecaCustomSystemExtractAll_R(Self: caEmutecaCustomSystem;
  var T: boolean);
begin
  T := Self.ExtractAll;
end;

procedure caEmutecaCustomSystemExtractAll_W(Self: caEmutecaCustomSystem;
  const T: boolean);
begin
  Self.ExtractAll := T;
end;

procedure caEmutecaCustomSystemBaseFolder_R(Self: caEmutecaCustomSystem;
  var T: string);
begin
  T := Self.BaseFolder;
end;

procedure caEmutecaCustomSystemBaseFolder_W(Self: caEmutecaCustomSystem;
  const T: string);
begin
  Self.BaseFolder := T;
end;

procedure caEmutecaCustomSystemWorkingFolder_R(Self: caEmutecaCustomSystem;
  var T: string);
begin
  T := Self.WorkingFolder;
end;

procedure caEmutecaCustomSystemWorkingFolder_W(Self: caEmutecaCustomSystem;
  const T: string);
begin
  Self.WorkingFolder := T;
end;

procedure caEmutecaCustomSystemMainEmulator_R(Self: caEmutecaCustomSystem;
  var T: string);
begin
  T := Self.MainEmulator;
end;

procedure caEmutecaCustomSystemMainEmulator_W(Self: caEmutecaCustomSystem;
  const T: string);
begin
  Self.MainEmulator := T;
end;

procedure caEmutecaCustomSystemOtherEmulators_R(Self: caEmutecaCustomSystem;
  var T: TStringList);
begin
  T := Self.OtherEmulators;
end;

procedure caEmutecaCustomSystemCoreIDs_R(Self: caEmutecaCustomSystem;
  var T: TStringList);
begin
  T := Self.CoreIDs;
end;

procedure caEmutecaCustomSystemIconFile_R(Self: caEmutecaCustomSystem;
  var T: string);
begin
  T := Self.IconFile;
end;

procedure caEmutecaCustomSystemIconFile_W(Self: caEmutecaCustomSystem;
  const T: string);
begin
  Self.IconFile := T;
end;

procedure caEmutecaCustomSystemImageFile_R(Self: caEmutecaCustomSystem;
  var T: string);
begin
  T := Self.ImageFile;
end;

procedure caEmutecaCustomSystemImageFile_W(Self: caEmutecaCustomSystem;
  const T: string);
begin
  Self.ImageFile := T;
end;

procedure caEmutecaCustomSystemSoftIconFile_R(Self: caEmutecaCustomSystem;
  var T: string);
begin
  T := Self.SoftIconFile;
end;

procedure caEmutecaCustomSystemSoftIconFile_W(Self: caEmutecaCustomSystem;
  const T: string);
begin
  Self.SoftIconFile := T;
end;

procedure caEmutecaCustomSystemIconFolder_R(Self: caEmutecaCustomSystem;
  var T: string);
begin
  T := Self.IconFolder;
end;

procedure caEmutecaCustomSystemIconFolder_W(Self: caEmutecaCustomSystem;
  const T: string);
begin
  Self.IconFolder := T;
end;

procedure caEmutecaCustomSystemLogoFolder_R(Self: caEmutecaCustomSystem;
  var T: string);
begin
  T := Self.LogoFolder;
end;

procedure caEmutecaCustomSystemLogoFolder_W(Self: caEmutecaCustomSystem;
  const T: string);
begin
  Self.LogoFolder := T;
end;

procedure caEmutecaCustomSystemImageFolders_R(Self: caEmutecaCustomSystem;
  var T: TStringList);
begin
  T := Self.ImageFolders;
end;

procedure caEmutecaCustomSystemImageCaptions_R(Self: caEmutecaCustomSystem;
  var T: TStringList);
begin
  T := Self.ImageCaptions;
end;

procedure caEmutecaCustomSystemInfoText_R(Self: caEmutecaCustomSystem;
  var T: string);
begin
  T := Self.InfoText;
end;

procedure caEmutecaCustomSystemInfoText_W(Self: caEmutecaCustomSystem;
  const T: string);
begin
  Self.InfoText := T;
end;

procedure caEmutecaCustomSystemTextFolders_R(Self: caEmutecaCustomSystem;
  var T: TStringList);
begin
  T := Self.TextFolders;
end;

procedure caEmutecaCustomSystemTextCaptions_R(Self: caEmutecaCustomSystem;
  var T: TStringList);
begin
  T := Self.TextCaptions;
end;

procedure caEmutecaCustomSystemMusicFolders_R(Self: caEmutecaCustomSystem;
  var T: TStringList);
begin
  T := Self.MusicFolders;
end;

procedure caEmutecaCustomSystemMusicCaptions_R(Self: caEmutecaCustomSystem;
  var T: TStringList);
begin
  T := Self.MusicCaptions;
end;

procedure caEmutecaCustomSystemVideoFolders_R(Self: caEmutecaCustomSystem;
  var T: TStringList);
begin
  T := Self.VideoFolders;
end;

procedure caEmutecaCustomSystemVideoCaptions_R(Self: caEmutecaCustomSystem;
  var T: TStringList);
begin
  T := Self.VideoCaptions;
end;

procedure caEmutecaCustomSystemOtherFolders_R(Self: caEmutecaCustomSystem;
  var T: TStringList);
begin
  T := Self.OtherFolders;
end;

procedure caEmutecaCustomSystemOtherFExt_R(Self: caEmutecaCustomSystem;
  var T: TStringList);
begin
  T := Self.OtherFExt;
end;

procedure caEmutecaCustomSystemOtherFCapt_R(Self: caEmutecaCustomSystem;
  var T: TStringList);
begin
  T := Self.OtherFCapt;
end;

procedure caEmutecaCustomSystemSoftExportKey_R(Self: caEmutecaCustomSystem;
  var T: TEmutecaSoftExportKey);
begin
  T := Self.SoftExportKey;
end;

procedure caEmutecaCustomSystemSoftExportKey_W(Self: caEmutecaCustomSystem;
  const T: TEmutecaSoftExportKey);
begin
  Self.SoftExportKey := T;
end;

procedure caEmutecaCustomSystemExtensions_R(Self: caEmutecaCustomSystem;
  var T: TStringList);
begin
  T := Self.Extensions;
end;

procedure caEmutecaCustomSystemStats_R(Self: caEmutecaCustomSystem;
  var T: cEmutecaPlayingStats);
begin
  T := Self.Stats;
end;

procedure RIRegister_caEmutecaCustomSystem(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(caEmutecaCustomSystem) do
  begin
    RegisterPropertyHelper(@caEmutecaCustomSystemTempFolder_R,
      @caEmutecaCustomSystemTempFolder_W, 'TempFolder');

    RegisterPropertyHelper(@caEmutecaCustomSystemID_R,
      @caEmutecaCustomSystemID_W, 'ID');
    RegisterPropertyHelper(@caEmutecaCustomSystemTitle_R,
      @caEmutecaCustomSystemTitle_W, 'Title');
    RegisterPropertyHelper(@caEmutecaCustomSystemListFileName_R,
      @caEmutecaCustomSystemListFileName_W, 'ListFileName');
    RegisterPropertyHelper(@caEmutecaCustomSystemEnabled_R,
      @caEmutecaCustomSystemEnabled_W, 'Enabled');
    RegisterPropertyHelper(@caEmutecaCustomSystemExtractAll_R,
      @caEmutecaCustomSystemExtractAll_W, 'ExtractAll');
    RegisterPropertyHelper(@caEmutecaCustomSystemBaseMergeableGroups_R,
      @caEmutecaCustomSystemBaseMergeableGroups_W, 'MergeableGroups');
    RegisterPropertyHelper(@caEmutecaCustomSystemBaseFolder_R,
      @caEmutecaCustomSystemBaseFolder_W, 'BaseFolder');
    RegisterPropertyHelper(@caEmutecaCustomSystemWorkingFolder_R,
      @caEmutecaCustomSystemWorkingFolder_W, 'WorkingFolder');

    RegisterPropertyHelper(@caEmutecaCustomSystemMainEmulator_R,
      @caEmutecaCustomSystemMainEmulator_W, 'MainEmulator');
    RegisterPropertyHelper(@caEmutecaCustomSystemOtherEmulators_R,
      nil, 'OtherEmulators');
    RegisterPropertyHelper(@caEmutecaCustomSystemCoreIDs_R,
      nil, 'CoreIDs');

    RegisterPropertyHelper(@caEmutecaCustomSystemIconFile_R,
      @caEmutecaCustomSystemIconFile_W, 'IconFile');
    RegisterPropertyHelper(@caEmutecaCustomSystemImageFile_R,
      @caEmutecaCustomSystemImageFile_W, 'ImageFile');
    RegisterPropertyHelper(@caEmutecaCustomSystemSoftIconFile_R,
      @caEmutecaCustomSystemSoftIconFile_W, 'SoftIconFile');

    RegisterPropertyHelper(@caEmutecaCustomSystemIconFolder_R,
      @caEmutecaCustomSystemIconFolder_W, 'IconFolder');
    RegisterPropertyHelper(@caEmutecaCustomSystemLogoFolder_R,
      @caEmutecaCustomSystemLogoFolder_W, 'LogoFolder');
    RegisterPropertyHelper(@caEmutecaCustomSystemImageFolders_R,
      nil, 'ImageFolders');
    RegisterPropertyHelper(@caEmutecaCustomSystemImageCaptions_R,
      nil, 'ImageCaptions');

    RegisterPropertyHelper(@caEmutecaCustomSystemInfoText_R,
      @caEmutecaCustomSystemInfoText_W, 'InfoText');
    RegisterPropertyHelper(@caEmutecaCustomSystemTextFolders_R,
      nil, 'TextFolders');
    RegisterPropertyHelper(@caEmutecaCustomSystemTextCaptions_R,
      nil, 'TextCaptions');

    RegisterPropertyHelper(@caEmutecaCustomSystemMusicFolders_R,
      nil, 'MusicFolders');
    RegisterPropertyHelper(@caEmutecaCustomSystemMusicCaptions_R,
      nil, 'MusicCaptions');

    RegisterPropertyHelper(@caEmutecaCustomSystemVideoFolders_R,
      nil, 'VideoFolders');
    RegisterPropertyHelper(@caEmutecaCustomSystemVideoCaptions_R,
      nil, 'VideoCaptions');

    RegisterPropertyHelper(@caEmutecaCustomSystemOtherFolders_R,
      nil, 'OtherFolders');
    RegisterPropertyHelper(@caEmutecaCustomSystemOtherFExt_R,
      nil, 'OtherFExt');
    RegisterPropertyHelper(@caEmutecaCustomSystemOtherFCapt_R,
      nil, 'OtherFCapt');

    RegisterPropertyHelper(@caEmutecaCustomSystemSoftExportKey_R,
      @caEmutecaCustomSystemSoftExportKey_W, 'SoftExportKey');
    RegisterPropertyHelper(@caEmutecaCustomSystemExtensions_R,
      nil, 'Extensions');
    RegisterPropertyHelper(@caEmutecaCustomSystemStats_R, nil, 'Stats');

    RegisterMethod(@caEmutecaCustomSystem.CompareID, 'CompareID');
    RegisterMethod(@caEmutecaCustomSystem.MatchID, 'MatchID');
  end;
end;

procedure RIRegister_uaEmutecaCustomSystem(CL: TPSRuntimeClassImporter);
begin
  RIRegister_caEmutecaCustomSystem(CL);
end;

{ TPSImport_uaEmutecaCustomSystem }
procedure TPSImport_uaEmutecaCustomSystem.CompileImport1(CompExec: TPSScript);
begin
  SIRegister_uaEmutecaCustomSystem(CompExec.comp);
end;

procedure TPSImport_uaEmutecaCustomSystem.ExecImport1(CompExec: TPSScript;
  const ri: TPSRuntimeClassImporter);
begin
  RIRegister_uaEmutecaCustomSystem(ri);
end;

end.
{
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
