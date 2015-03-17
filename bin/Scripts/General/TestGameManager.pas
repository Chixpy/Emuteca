{
[SCRIPTDATA]
Author = Chixpy
Version = 0.3
Date = 20101109
Description =
~begin~
  Script de prueba, para probar cGameManager.
~end~
Changes =
~begin~
  0.4 - 20111116:
    m Adapting to cGameManeger changes.
  0.3 - 20101109:
    m Little tweaks.
  0.2 - 20101025:
    + Añadida comprobación de que "GameManager" está definido.
    + Muestra addemás el número de juegos.
  0.1 - 20101023:
    + Versión inicial.
    + Escribe el número de grupos que hay en el GameManager.
~end~
[END]
}
program TestGameManager;
begin
  WriteLn('GAMEMANAGER PROPERTIES');
  WriteLn('----------------------');
  WriteLn('GameManager.SystemsFile: ' + GameManager.SystemsFile);
  WriteLn('GameManager.EmulatorsFile: ' + GameManager.EmulatorsFile);
  WriteLn('GameManager.GameDataFileExt: ' + GameManager.GameDataFileExt);
  WriteLn('GameManager.GroupDataFileExt: ' + GameManager.GroupDataFileExt);
  // TODO 1: We need to export cSystem to Pascal Script
  // WriteLn('GameManager.System.ID: ' + GameManager.System.ID);
  // TODO 1: We need to export cEmulator to Pascal Script
  // WriteLn('GameManager.Emulator.ID: ' + GameManager.System.ID);
  WriteLn('GameManager.TempFolder: ' + GameManager.TempFolder);
  WriteLn('GameManager.TempFile: ' + GameManager.TempFile);
  WriteLn('Check CRC for files smaller than: ' +
    IntToStr(GameManager.CRCMaxSize) + ' bytes. (GameManager.CRCMaxSize)');
  WriteLn('Compressed file extensions (GameManager.CompressedExt.CommaText): ' + GameManager.CompressedExt.CommaText);
  WriteLn('');
  WriteLn('GAMEMANAGER METHODS');
  WriteLn('-------------------');
  WriteLn('GameManager.GroupCount: '+ IntToStr(GameManager.GroupCount));
  WriteLn('GameManager.GameCount: '+ IntToStr(GameManager.GameCount));

  if GameManager.GroupCount > 0 then
  begin
    WriteLn('');
    WriteLn('The first group is: ' + GameManager.GroupAtPos(0).Name);
  end;
  if GameManager.GameCount > 0 then
  begin
    WriteLn('');
    WriteLn('The first game is: ' + GameManager.GameAtPos(0).Name);
  end;
end.
