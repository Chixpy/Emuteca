unit uPSI_ucEmutecaPlayingStats;

{< cEmutecaPlayingStats import for Pascal Script.

  This file is part of Emuteca Core.

  Copyright (C) 2023 Chixpy

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
interface

uses
  SysUtils, Classes, uPSComponent, uPSRuntime, uPSCompiler, Graphics,
  // Emuteca Core abstracts
  ucEmutecaPlayingStats;

type
  (*----------------------------------------------------------------------------*)
  TPSImport_ucEmutecaPlayingStats = class(TPSPlugin)
    procedure CompileImport1(CompExec: TPSScript); override;
    procedure ExecImport1(CompExec: TPSScript;
      const ri: TPSRuntimeClassImporter); override;
  end;


{ compile-time registration functions }
procedure SIRegister_cEmutecaPlayingStats(CL: TPSPascalCompiler);
procedure SIRegister_ucEmutecaPlayingStats(CL: TPSPascalCompiler);

{ run-time registration functions }
procedure RIRegister_cEmutecaPlayingStats(CL: TPSRuntimeClassImporter);
procedure RIRegister_ucEmutecaPlayingStats(CL: TPSRuntimeClassImporter);

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Pascal Script', [TPSImport_ucEmutecaPlayingStats]);
end;

procedure SIRegister_cEmutecaPlayingStats(CL: TPSPascalCompiler);
begin
  //with RegClassS(CL,'TComponent', 'cEmutecaPlayingStats') do
  with CL.AddClassN(CL.FindClass('TComponent'), 'cEmutecaPlayingStats') do
  begin
    RegisterProperty('LastTime', 'TDateTime', iptrw);
    RegisterProperty('TimesPlayed', 'int64', iptrw);
    RegisterProperty('PlayingTime', 'int64', iptrw);

    RegisterProperty('Icon', 'TPicture', iptrw);
    RegisterProperty('SysSoftIcon', 'TPicture', iptrw);

    RegisterMethod('procedure AddPlayingTime(const Start: TDateTime; NumberOfSeconds: int64);');

    RegisterMethod('function LastTimeStr: string;');
    RegisterMethod('function TimesPlayedStr: string;');
    RegisterMethod('function PlayingTimeStr: string;');
  end;
end;

procedure SIRegister_ucEmutecaPlayingStats(CL: TPSPascalCompiler);
begin
  SIRegister_cEmutecaPlayingStats(CL);
end;

procedure cEmutecaPlayingStatsLastTime_R(Self: cEmutecaPlayingStats;
  var T: TDateTime);
begin
  T := Self.LastTime;
end;

procedure cEmutecaPlayingStatsLastTime_W(Self: cEmutecaPlayingStats;
  const T: TDateTime);
begin
  Self.LastTime := T;
end;

procedure cEmutecaPlayingStatsTimesPlayed_R(Self: cEmutecaPlayingStats;
  var T: int64);
begin
  T := Self.TimesPlayed;
end;

procedure cEmutecaPlayingStatsTimesPlayed_W(Self: cEmutecaPlayingStats;
  const T: int64);
begin
  Self.TimesPlayed := T;
end;

procedure cEmutecaPlayingStatsPlayingTime_R(Self: cEmutecaPlayingStats;
  var T: int64);
begin
  T := Self.PlayingTime;
end;

procedure cEmutecaPlayingStatsPlayingTime_W(Self: cEmutecaPlayingStats;
  const T: int64);
begin
  Self.PlayingTime := T;
end;

procedure cEmutecaPlayingStatsIcon_R(Self: cEmutecaPlayingStats; var T: TPicture);
begin
  T := Self.Icon;
end;

procedure cEmutecaPlayingStatsIcon_W(Self: cEmutecaPlayingStats;
  const T: TPicture);
begin
  Self.Icon := T;
end;

procedure cEmutecaPlayingStatsSysSoftIcon_R(Self: cEmutecaPlayingStats;
  var T: TPicture);
begin
  T := Self.SysSoftIcon;
end;

procedure cEmutecaPlayingStatsSysSoftIcon_W(Self: cEmutecaPlayingStats;
  const T: TPicture);
begin
  Self.SysSoftIcon := T;
end;

procedure RIRegister_cEmutecaPlayingStats(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(cEmutecaPlayingStats) do
  begin
    RegisterPropertyHelper(@cEmutecaPlayingStatsIcon_R,
      @cEmutecaPlayingStatsIcon_W, 'Icon');
    RegisterPropertyHelper(@cEmutecaPlayingStatsSysSoftIcon_R,
      @cEmutecaPlayingStatsSysSoftIcon_W, 'SysSoftIcon');

    RegisterPropertyHelper(@cEmutecaPlayingStatsLastTime_R,
      @cEmutecaPlayingStatsLastTime_W, 'LastTime');
    RegisterPropertyHelper(@cEmutecaPlayingStatsTimesPlayed_R,
      @cEmutecaPlayingStatsTimesPlayed_W, 'TimesPlayed');
    RegisterPropertyHelper(@cEmutecaPlayingStatsPlayingTime_R,
      @cEmutecaPlayingStatsPlayingTime_W, 'PlayingTime');

    RegisterMethod(@cEmutecaPlayingStats.AddPlayingTime, 'AddPlayingTime');

    RegisterMethod(@cEmutecaPlayingStats.LastTimeStr, 'LastTimeStr');
    RegisterMethod(@cEmutecaPlayingStats.TimesPlayedStr, 'TimesPlayedStr');
    RegisterMethod(@cEmutecaPlayingStats.PlayingTimeStr, 'PlayingTimeStr');
  end;
end;

procedure RIRegister_ucEmutecaPlayingStats(CL: TPSRuntimeClassImporter);
begin
  RIRegister_cEmutecaPlayingStats(CL);
end;


{ TPSImport_ucEmutecaPlayingStats }

procedure TPSImport_ucEmutecaPlayingStats.CompileImport1(CompExec: TPSScript);
begin
  SIRegister_ucEmutecaPlayingStats(CompExec.comp);
end;

(*----------------------------------------------------------------------------*)
procedure TPSImport_ucEmutecaPlayingStats.ExecImport1(CompExec: TPSScript;
  const ri: TPSRuntimeClassImporter);
begin
  RIRegister_ucEmutecaPlayingStats(ri);
end;

end.
