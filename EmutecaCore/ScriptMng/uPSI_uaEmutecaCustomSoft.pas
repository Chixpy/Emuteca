unit uPSI_uaEmutecaCustomSoft;
interface
 
uses
   SysUtils
  ,Classes
  ,uPSComponent
  ,uPSRuntime
  ,uPSCompiler
  ;
 
type 
(*----------------------------------------------------------------------------*)
  TPSImport_uaEmutecaCustomSoft = class(TPSPlugin)
  protected
    procedure CompileImport1(CompExec: TPSScript); override;
    procedure ExecImport1(CompExec: TPSScript; const ri: TPSRuntimeClassImporter); override;
  end;
 
 
{ compile-time registration functions }
procedure SIRegister_caEmutecaCustomSoft(CL: TPSPascalCompiler);
procedure SIRegister_uaEmutecaCustomSoft(CL: TPSPascalCompiler);

{ run-time registration functions }
procedure RIRegister_caEmutecaCustomSoft(CL: TPSRuntimeClassImporter);
procedure RIRegister_uaEmutecaCustomSoft(CL: TPSRuntimeClassImporter);

procedure Register;

implementation


uses
   IniFiles
  ,sha1
  ,LazUTF8
  ,LazFileUtils
  ,uCHXStrUtils
  ,uCHXFileUtils
  ,uaCHXStorable
  ,uEmutecaCommon
  ,ucEmutecaPlayingStats
  ,uaEmutecaCustomSoft
  ;
 
 
procedure Register;
begin
  RegisterComponents('Pascal Script', [TPSImport_uaEmutecaCustomSoft]);
end;

(* === compile-time registration functions === *)
(*----------------------------------------------------------------------------*)
procedure SIRegister_caEmutecaCustomSoft(CL: TPSPascalCompiler);
begin
  //with RegClassS(CL,'caCHXStorableTxt', 'caEmutecaCustomSoft') do
  with CL.AddClassN(CL.FindClass('caCHXStorableTxt'),'caEmutecaCustomSoft') do
  begin
    RegisterProperty('SHA1', 'TSHA1Digest', iptrw);
    RegisterMethod('Function GetActualID : string');
    RegisterMethod('Function GetActualTitle : string');
    RegisterMethod('Function GetActualSortTitle : string');
    RegisterMethod('Function GetActualTranslitTitle : string');
    RegisterMethod('Function SHA1IsEmpty : boolean');
    RegisterMethod('Function MatchSHA1( aSHA1 : TSHA1Digest) : boolean');
    RegisterMethod('Function MatchFile( aFolder, aFile : string) : boolean');
    RegisterMethod('Function MatchID( aID : string) : boolean');
    RegisterMethod('Function CompareID( aID : string) : integer');
    RegisterMethod('Function MatchGroupKey( aGroupID : string) : boolean');
    RegisterMethod('Function CompareGroupKey( aGroupID : string) : integer');
    RegisterMethod('Function MatchGroupFile : boolean');
    RegisterMethod('Procedure ImportFrom( aSoft : caEmutecaCustomSoft)');
    RegisterMethod('Procedure SearchAllRelatedFiles( OutFileList : TStrings; aFolder : string; Extensions : TStrings; AutoExtract : boolean)');
    RegisterMethod('Function SearchFirstRelatedFile( aFolder : string; Extensions : TStrings; AutoExtract : boolean) : string');
    RegisterProperty('ID', 'string', iptrw);
    RegisterProperty('Folder', 'string', iptrw);
    RegisterProperty('FileName', 'string', iptrw);
    RegisterProperty('GroupKey', 'string', iptrw);
    RegisterProperty('Title', 'string', iptrw);
    RegisterProperty('TranslitTitle', 'string', iptrw);
    RegisterProperty('SortTitle', 'string', iptrw);
    RegisterProperty('Version', 'string', iptrw);
    RegisterProperty('Year', 'string', iptrw);
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
    RegisterProperty('Stats', 'cEmutecaPlayingStats', iptr);
  end;
end;

(*----------------------------------------------------------------------------*)
procedure SIRegister_uaEmutecaCustomSoft(CL: TPSPascalCompiler);
begin
  SIRegister_caEmutecaCustomSoft(CL);
end;

(* === run-time registration functions === *)
(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSoftStats_R(Self: caEmutecaCustomSoft; var T: cEmutecaPlayingStats);
begin T := Self.Stats; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSoftHack_W(Self: caEmutecaCustomSoft; const T: string);
begin Self.Hack := T; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSoftHack_R(Self: caEmutecaCustomSoft; var T: string);
begin T := Self.Hack; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSoftModified_W(Self: caEmutecaCustomSoft; const T: string);
begin Self.Modified := T; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSoftModified_R(Self: caEmutecaCustomSoft; var T: string);
begin T := Self.Modified; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSoftCracked_W(Self: caEmutecaCustomSoft; const T: string);
begin Self.Cracked := T; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSoftCracked_R(Self: caEmutecaCustomSoft; var T: string);
begin T := Self.Cracked; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSoftPirate_W(Self: caEmutecaCustomSoft; const T: string);
begin Self.Pirate := T; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSoftPirate_R(Self: caEmutecaCustomSoft; var T: string);
begin T := Self.Pirate; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSoftTranslation_W(Self: caEmutecaCustomSoft; const T: string);
begin Self.Translation := T; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSoftTranslation_R(Self: caEmutecaCustomSoft; var T: string);
begin T := Self.Translation; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSoftTrainer_W(Self: caEmutecaCustomSoft; const T: string);
begin Self.Trainer := T; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSoftTrainer_R(Self: caEmutecaCustomSoft; var T: string);
begin T := Self.Trainer; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSoftFixed_W(Self: caEmutecaCustomSoft; const T: string);
begin Self.Fixed := T; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSoftFixed_R(Self: caEmutecaCustomSoft; var T: string);
begin T := Self.Fixed; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSoftDumpInfo_W(Self: caEmutecaCustomSoft; const T: string);
begin Self.DumpInfo := T; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSoftDumpInfo_R(Self: caEmutecaCustomSoft; var T: string);
begin T := Self.DumpInfo; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSoftDumpStatus_W(Self: caEmutecaCustomSoft; const T: TEmutecaDumpStatus);
begin Self.DumpStatus := T; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSoftDumpStatus_R(Self: caEmutecaCustomSoft; var T: TEmutecaDumpStatus);
begin T := Self.DumpStatus; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSoftZone_W(Self: caEmutecaCustomSoft; const T: string);
begin Self.Zone := T; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSoftZone_R(Self: caEmutecaCustomSoft; var T: string);
begin T := Self.Zone; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSoftPublisher_W(Self: caEmutecaCustomSoft; const T: string);
begin Self.Publisher := T; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSoftPublisher_R(Self: caEmutecaCustomSoft; var T: string);
begin T := Self.Publisher; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSoftYear_W(Self: caEmutecaCustomSoft; const T: string);
begin Self.Year := T; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSoftYear_R(Self: caEmutecaCustomSoft; var T: string);
begin T := Self.Year; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSoftVersion_W(Self: caEmutecaCustomSoft; const T: string);
begin Self.Version := T; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSoftVersion_R(Self: caEmutecaCustomSoft; var T: string);
begin T := Self.Version; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSoftSortTitle_W(Self: caEmutecaCustomSoft; const T: string);
begin Self.SortTitle := T; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSoftSortTitle_R(Self: caEmutecaCustomSoft; var T: string);
begin T := Self.SortTitle; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSoftTranslitTitle_W(Self: caEmutecaCustomSoft; const T: string);
begin Self.TranslitTitle := T; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSoftTranslitTitle_R(Self: caEmutecaCustomSoft; var T: string);
begin T := Self.TranslitTitle; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSoftTitle_W(Self: caEmutecaCustomSoft; const T: string);
begin Self.Title := T; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSoftTitle_R(Self: caEmutecaCustomSoft; var T: string);
begin T := Self.Title; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSoftGroupKey_W(Self: caEmutecaCustomSoft; const T: string);
begin Self.GroupKey := T; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSoftGroupKey_R(Self: caEmutecaCustomSoft; var T: string);
begin T := Self.GroupKey; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSoftFileName_W(Self: caEmutecaCustomSoft; const T: string);
begin Self.FileName := T; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSoftFileName_R(Self: caEmutecaCustomSoft; var T: string);
begin T := Self.FileName; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSoftFolder_W(Self: caEmutecaCustomSoft; const T: string);
begin Self.Folder := T; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSoftFolder_R(Self: caEmutecaCustomSoft; var T: string);
begin T := Self.Folder; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSoftID_W(Self: caEmutecaCustomSoft; const T: string);
begin Self.ID := T; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSoftID_R(Self: caEmutecaCustomSoft; var T: string);
begin T := Self.ID; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSoftSHA1_W(Self: caEmutecaCustomSoft; const T: TSHA1Digest);
begin Self.SHA1 := T; end;

(*----------------------------------------------------------------------------*)
procedure caEmutecaCustomSoftSHA1_R(Self: caEmutecaCustomSoft; var T: TSHA1Digest);
begin T := Self.SHA1; end;

(*----------------------------------------------------------------------------*)
procedure RIRegister_caEmutecaCustomSoft(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(caEmutecaCustomSoft) do
  begin
    RegisterPropertyHelper(@caEmutecaCustomSoftSHA1_R,@caEmutecaCustomSoftSHA1_W,'SHA1');
    RegisterMethod(@caEmutecaCustomSoft.GetActualID, 'GetActualID');
    RegisterMethod(@caEmutecaCustomSoft.GetActualTitle, 'GetActualTitle');
    RegisterMethod(@caEmutecaCustomSoft.GetActualSortTitle, 'GetActualSortTitle');
    RegisterMethod(@caEmutecaCustomSoft.GetActualTranslitTitle, 'GetActualTranslitTitle');
    RegisterMethod(@caEmutecaCustomSoft.SHA1IsEmpty, 'SHA1IsEmpty');
    RegisterMethod(@caEmutecaCustomSoft.MatchSHA1, 'MatchSHA1');
    RegisterMethod(@caEmutecaCustomSoft.MatchFile, 'MatchFile');
    RegisterMethod(@caEmutecaCustomSoft.MatchID, 'MatchID');
    RegisterMethod(@caEmutecaCustomSoft.CompareID, 'CompareID');
    RegisterMethod(@caEmutecaCustomSoft.MatchGroupKey, 'MatchGroupKey');
    RegisterMethod(@caEmutecaCustomSoft.CompareGroupKey, 'CompareGroupKey');
    RegisterVirtualMethod(@caEmutecaCustomSoft.MatchGroupFile, 'MatchGroupFile');
    RegisterMethod(@caEmutecaCustomSoft.ImportFrom, 'ImportFrom');
    RegisterVirtualMethod(@caEmutecaCustomSoft.SearchAllRelatedFiles, 'SearchAllRelatedFiles');
    RegisterVirtualMethod(@caEmutecaCustomSoft.SearchFirstRelatedFile, 'SearchFirstRelatedFile');
    RegisterPropertyHelper(@caEmutecaCustomSoftID_R,@caEmutecaCustomSoftID_W,'ID');
    RegisterPropertyHelper(@caEmutecaCustomSoftFolder_R,@caEmutecaCustomSoftFolder_W,'Folder');
    RegisterPropertyHelper(@caEmutecaCustomSoftFileName_R,@caEmutecaCustomSoftFileName_W,'FileName');
    RegisterPropertyHelper(@caEmutecaCustomSoftGroupKey_R,@caEmutecaCustomSoftGroupKey_W,'GroupKey');
    RegisterPropertyHelper(@caEmutecaCustomSoftTitle_R,@caEmutecaCustomSoftTitle_W,'Title');
    RegisterPropertyHelper(@caEmutecaCustomSoftTranslitTitle_R,@caEmutecaCustomSoftTranslitTitle_W,'TranslitTitle');
    RegisterPropertyHelper(@caEmutecaCustomSoftSortTitle_R,@caEmutecaCustomSoftSortTitle_W,'SortTitle');
    RegisterPropertyHelper(@caEmutecaCustomSoftVersion_R,@caEmutecaCustomSoftVersion_W,'Version');
    RegisterPropertyHelper(@caEmutecaCustomSoftYear_R,@caEmutecaCustomSoftYear_W,'Year');
    RegisterPropertyHelper(@caEmutecaCustomSoftPublisher_R,@caEmutecaCustomSoftPublisher_W,'Publisher');
    RegisterPropertyHelper(@caEmutecaCustomSoftZone_R,@caEmutecaCustomSoftZone_W,'Zone');
    RegisterPropertyHelper(@caEmutecaCustomSoftDumpStatus_R,@caEmutecaCustomSoftDumpStatus_W,'DumpStatus');
    RegisterPropertyHelper(@caEmutecaCustomSoftDumpInfo_R,@caEmutecaCustomSoftDumpInfo_W,'DumpInfo');
    RegisterPropertyHelper(@caEmutecaCustomSoftFixed_R,@caEmutecaCustomSoftFixed_W,'Fixed');
    RegisterPropertyHelper(@caEmutecaCustomSoftTrainer_R,@caEmutecaCustomSoftTrainer_W,'Trainer');
    RegisterPropertyHelper(@caEmutecaCustomSoftTranslation_R,@caEmutecaCustomSoftTranslation_W,'Translation');
    RegisterPropertyHelper(@caEmutecaCustomSoftPirate_R,@caEmutecaCustomSoftPirate_W,'Pirate');
    RegisterPropertyHelper(@caEmutecaCustomSoftCracked_R,@caEmutecaCustomSoftCracked_W,'Cracked');
    RegisterPropertyHelper(@caEmutecaCustomSoftModified_R,@caEmutecaCustomSoftModified_W,'Modified');
    RegisterPropertyHelper(@caEmutecaCustomSoftHack_R,@caEmutecaCustomSoftHack_W,'Hack');
    RegisterPropertyHelper(@caEmutecaCustomSoftStats_R,nil,'Stats');
  end;
end;

(*----------------------------------------------------------------------------*)
procedure RIRegister_uaEmutecaCustomSoft(CL: TPSRuntimeClassImporter);
begin
  RIRegister_caEmutecaCustomSoft(CL);
end;

 
 
{ TPSImport_uaEmutecaCustomSoft }
(*----------------------------------------------------------------------------*)
procedure TPSImport_uaEmutecaCustomSoft.CompileImport1(CompExec: TPSScript);
begin
  SIRegister_uaEmutecaCustomSoft(CompExec.Comp);
end;
(*----------------------------------------------------------------------------*)
procedure TPSImport_uaEmutecaCustomSoft.ExecImport1(CompExec: TPSScript; const ri: TPSRuntimeClassImporter);
begin
  RIRegister_uaEmutecaCustomSoft(ri);
end;
(*----------------------------------------------------------------------------*)
 
 
end.
