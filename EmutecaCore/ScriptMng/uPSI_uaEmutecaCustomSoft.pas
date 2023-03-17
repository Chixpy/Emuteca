unit uPSI_uaEmutecaCustomSoft;
{< caEmutecaCustomSoft export for Pascal Script.

  Copyright (C) 2018-2023 Chixpy

  This source is free software; you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 3 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
  more details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by
  writing to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
  Boston, MA 02111-1307, USA.
}
interface

uses
  SysUtils, Classes, uPSComponent, uPSRuntime, uPSCompiler, sha1,
  // Emuteca Core units
  uEmutecaConst,
  // Emuteca Core classes
  ucEmutecaPlayingStats, uaEmutecaCustomSoft;

type
  TPSImport_uaEmutecaCustomSoft = class(TPSPlugin)
  public
    procedure CompileImport1(CompExec: TPSScript); override;
    procedure ExecImport1(CompExec: TPSScript;
      const ri: TPSRuntimeClassImporter); override;
  end;


procedure SIRegister_caEmutecaCustomSoft(CL: TPSPascalCompiler);
procedure SIRegister_uaEmutecaCustomSoft(CL: TPSPascalCompiler);

procedure RIRegister_caEmutecaCustomSoft(CL: TPSRuntimeClassImporter);
procedure RIRegister_uaEmutecaCustomSoft(CL: TPSRuntimeClassImporter);

procedure Register;


implementation

procedure Register;
begin
  RegisterComponents('Pascal Script', [TPSImport_uaEmutecaCustomSoft]);
end;

procedure SIRegister_caEmutecaCustomSoft(CL: TPSPascalCompiler);
begin
  //with RegClassS(CL,'caEmutecaCustomSGItem', 'caEmutecaCustomSoft') do
  with CL.AddClassN(CL.FindClass('caEmutecaCustomSGItem'),
      'caEmutecaCustomSoft') do
  begin
    RegisterProperty('SHA1', 'TSHA1Digest', iptrw);

    RegisterProperty('Folder', 'string', iptrw);
    RegisterProperty('FileName', 'string', iptrw);
    RegisterProperty('GroupKey', 'string', iptrw);

    RegisterProperty('Version', 'string', iptrw);
    RegisterProperty('Publisher', 'string', iptrw);
    RegisterProperty('Zone', 'string', iptrw);

    RegisterProperty('DumpStatus', 'TEmutecaDumpStatus', iptrw);
    RegisterProperty('DumpInfo', 'string', iptrw);
    RegisterProperty('Fixed', 'string', iptrw);
    RegisterProperty('Trainer', 'string', iptrw);
    RegisterProperty('Translation', 'string', iptrw);
    RegisterProperty('Pirate', 'string', iptrw);
    RegisterProperty('Cracked', 'string', iptrw);
    RegisterProperty('Modified', 'string', iptrw);
    RegisterProperty('Hack', 'string', iptrw);
    RegisterProperty('ExtraParameters', 'TStringList', iptr);
    RegisterProperty('Stats', 'cEmutecaPlayingStats', iptr);

    RegisterMethod('function SHA1IsEmpty: boolean;');
    RegisterMethod('function MatchSHA1(aSHA1: TSHA1Digest): boolean;');
    RegisterMethod('function CompareFile(const aFolder, aFile: string): integer;');
    RegisterMethod('function MatchFile(const aFolder, aFile: string): boolean;');
    RegisterMethod('function MatchGroupKey(const aGroupID: string): boolean;');
    RegisterMethod('function CompareGroupKey(const aGroupID: string): integer;');
    RegisterMethod('function MatchGroupFile: boolean; virtual;');

    RegisterMethod('function ExportCommaText: string;');
    RegisterMethod('procedure ImportFrom(aSoft: caEmutecaCustomSoft);');
  end;
end;

procedure SIRegister_uaEmutecaCustomSoft(CL: TPSPascalCompiler);
begin
  SIRegister_caEmutecaCustomSoft(CL);
end;

procedure caEmutecaCustomSoftSHA1_R(Self: caEmutecaCustomSoft;
  var T: TSHA1Digest);
begin
  T := Self.SHA1;
end;

procedure caEmutecaCustomSoftSHA1_W(Self: caEmutecaCustomSoft;
  const T: TSHA1Digest);
begin
  Self.SHA1 := T;
end;

procedure caEmutecaCustomSoftFolder_R(Self: caEmutecaCustomSoft;
  var T: string);
begin
  T := Self.Folder;
end;

procedure caEmutecaCustomSoftFolder_W(Self: caEmutecaCustomSoft;
  const T: string);
begin
  Self.Folder := T;
end;

procedure caEmutecaCustomSoftFileName_R(Self: caEmutecaCustomSoft;
  var T: string);
begin
  T := Self.FileName;
end;

procedure caEmutecaCustomSoftFileName_W(Self: caEmutecaCustomSoft;
  const T: string);
begin
  Self.FileName := T;
end;

procedure caEmutecaCustomSoftGroupKey_R(Self: caEmutecaCustomSoft;
  var T: string);
begin
  T := Self.GroupKey;
end;

procedure caEmutecaCustomSoftGroupKey_W(Self: caEmutecaCustomSoft;
  const T: string);
begin
  Self.GroupKey := T;
end;

procedure caEmutecaCustomSoftVersion_R(Self: caEmutecaCustomSoft;
  var T: string);
begin
  T := Self.Version;
end;

procedure caEmutecaCustomSoftVersion_W(Self: caEmutecaCustomSoft;
  const T: string);
begin
  Self.Version := T;
end;

procedure caEmutecaCustomSoftPublisher_R(Self: caEmutecaCustomSoft;
  var T: string);
begin
  T := Self.Publisher;
end;

procedure caEmutecaCustomSoftPublisher_W(Self: caEmutecaCustomSoft;
  const T: string);
begin
  Self.Publisher := T;
end;

procedure caEmutecaCustomSoftZone_R(Self: caEmutecaCustomSoft; var T: string);
begin
  T := Self.Zone;
end;

procedure caEmutecaCustomSoftZone_W(Self: caEmutecaCustomSoft;
  const T: string);
begin
  Self.Zone := T;
end;

procedure caEmutecaCustomSoftDumpStatus_R(Self: caEmutecaCustomSoft;
  var T: TEmutecaDumpStatus);
begin
  T := Self.DumpStatus;
end;

procedure caEmutecaCustomSoftDumpStatus_W(Self: caEmutecaCustomSoft;
  const T: TEmutecaDumpStatus);
begin
  Self.DumpStatus := T;
end;

procedure caEmutecaCustomSoftDumpInfo_R(Self: caEmutecaCustomSoft;
  var T: string);
begin
  T := Self.DumpInfo;
end;

procedure caEmutecaCustomSoftDumpInfo_W(Self: caEmutecaCustomSoft;
  const T: string);
begin
  Self.DumpInfo := T;
end;

procedure caEmutecaCustomSoftFixed_R(Self: caEmutecaCustomSoft; var T: string);
begin
  T := Self.Fixed;
end;

procedure caEmutecaCustomSoftFixed_W(Self: caEmutecaCustomSoft;
  const T: string);
begin
  Self.Fixed := T;
end;

procedure caEmutecaCustomSoftTrainer_R(Self: caEmutecaCustomSoft;
  var T: string);
begin
  T := Self.Trainer;
end;

procedure caEmutecaCustomSoftTrainer_W(Self: caEmutecaCustomSoft;
  const T: string);
begin
  Self.Trainer := T;
end;

procedure caEmutecaCustomSoftTranslation_R(Self: caEmutecaCustomSoft;
  var T: string);
begin
  T := Self.Translation;
end;

procedure caEmutecaCustomSoftTranslation_W(Self: caEmutecaCustomSoft;
  const T: string);
begin
  Self.Translation := T;
end;

procedure caEmutecaCustomSoftPirate_R(Self: caEmutecaCustomSoft;
  var T: string);
begin
  T := Self.Pirate;
end;

procedure caEmutecaCustomSoftPirate_W(Self: caEmutecaCustomSoft;
  const T: string);
begin
  Self.Pirate := T;
end;

procedure caEmutecaCustomSoftCracked_R(Self: caEmutecaCustomSoft;
  var T: string);
begin
  T := Self.Cracked;
end;

procedure caEmutecaCustomSoftCracked_W(Self: caEmutecaCustomSoft;
  const T: string);
begin
  Self.Cracked := T;
end;

procedure caEmutecaCustomSoftModified_R(Self: caEmutecaCustomSoft;
  var T: string);
begin
  T := Self.Modified;
end;

procedure caEmutecaCustomSoftModified_W(Self: caEmutecaCustomSoft;
  const T: string);
begin
  Self.Modified := T;
end;

procedure caEmutecaCustomSoftHack_R(Self: caEmutecaCustomSoft; var T: string);
begin
  T := Self.Hack;
end;

procedure caEmutecaCustomSoftHack_W(Self: caEmutecaCustomSoft;
  const T: string);
begin
  Self.Hack := T;
end;

procedure caEmutecaCustomSoftExtraParameters_R(Self: caEmutecaCustomSoft;
  var T: TStringList);
begin
  T := Self.ExtraParameters;
end;

procedure caEmutecaCustomSoftStats_R(Self: caEmutecaCustomSoft;
  var T: cEmutecaPlayingStats);
begin
  T := Self.Stats;
end;

procedure RIRegister_caEmutecaCustomSoft(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(caEmutecaCustomSoft) do
  begin
    RegisterPropertyHelper(@caEmutecaCustomSoftSHA1_R,
      @caEmutecaCustomSoftSHA1_W, 'SHA1');

    RegisterPropertyHelper(@caEmutecaCustomSoftFolder_R,
      @caEmutecaCustomSoftFolder_W, 'Folder');
    RegisterPropertyHelper(@caEmutecaCustomSoftFileName_R,
      @caEmutecaCustomSoftFileName_W, 'FileName');
    RegisterPropertyHelper(@caEmutecaCustomSoftGroupKey_R,
      @caEmutecaCustomSoftGroupKey_W, 'GroupKey');

    RegisterPropertyHelper(@caEmutecaCustomSoftVersion_R,
      @caEmutecaCustomSoftVersion_W, 'Version');
    RegisterPropertyHelper(@caEmutecaCustomSoftPublisher_R,
      @caEmutecaCustomSoftPublisher_W, 'Publisher');
    RegisterPropertyHelper(@caEmutecaCustomSoftZone_R,
      @caEmutecaCustomSoftZone_W, 'Zone');

    RegisterPropertyHelper(@caEmutecaCustomSoftDumpStatus_R,
      @caEmutecaCustomSoftDumpStatus_W, 'DumpStatus');
    RegisterPropertyHelper(@caEmutecaCustomSoftDumpInfo_R,
      @caEmutecaCustomSoftDumpInfo_W, 'DumpInfo');
    RegisterPropertyHelper(@caEmutecaCustomSoftFixed_R,
      @caEmutecaCustomSoftFixed_W, 'Fixed');
    RegisterPropertyHelper(@caEmutecaCustomSoftTrainer_R,
      @caEmutecaCustomSoftTrainer_W, 'Trainer');
    RegisterPropertyHelper(@caEmutecaCustomSoftTranslation_R,
      @caEmutecaCustomSoftTranslation_W, 'Translation');
    RegisterPropertyHelper(@caEmutecaCustomSoftPirate_R,
      @caEmutecaCustomSoftPirate_W, 'Pirate');
    RegisterPropertyHelper(@caEmutecaCustomSoftCracked_R,
      @caEmutecaCustomSoftCracked_W, 'Cracked');
    RegisterPropertyHelper(@caEmutecaCustomSoftModified_R,
      @caEmutecaCustomSoftModified_W, 'Modified');
    RegisterPropertyHelper(@caEmutecaCustomSoftHack_R,
      @caEmutecaCustomSoftHack_W, 'Hack');
    RegisterPropertyHelper(@caEmutecaCustomSoftExtraParameters_R,
      nil, 'ExtraParameters');
    RegisterPropertyHelper(@caEmutecaCustomSoftStats_R, nil, 'Stats');

    RegisterMethod(@caEmutecaCustomSoft.SHA1IsEmpty, 'SHA1IsEmpty');
    RegisterMethod(@caEmutecaCustomSoft.MatchSHA1, 'MatchSHA1');
    RegisterMethod(@caEmutecaCustomSoft.CompareFile, 'CompareFile');
    RegisterMethod(@caEmutecaCustomSoft.MatchFile, 'MatchFile');
    RegisterMethod(@caEmutecaCustomSoft.MatchGroupKey, 'MatchGroupKey');
    RegisterMethod(@caEmutecaCustomSoft.CompareGroupKey, 'CompareGroupKey');
    RegisterVirtualMethod(@caEmutecaCustomSoft.MatchGroupFile,
      'MatchGroupFile');

    RegisterMethod(@caEmutecaCustomSoft.ExportCommaText, 'ExportCommaText');
    RegisterMethod(@caEmutecaCustomSoft.ImportFrom, 'ImportFrom');
  end;
end;

procedure RIRegister_uaEmutecaCustomSoft(CL: TPSRuntimeClassImporter);
begin
  RIRegister_caEmutecaCustomSoft(CL);
end;

procedure TPSImport_uaEmutecaCustomSoft.CompileImport1(CompExec: TPSScript);
begin
  SIRegister_uaEmutecaCustomSoft(CompExec.comp);
end;

procedure TPSImport_uaEmutecaCustomSoft.ExecImport1(CompExec: TPSScript;
  const ri: TPSRuntimeClassImporter);
begin
  RIRegister_uaEmutecaCustomSoft(ri);
end;

end.
