This is the whislist for Emuteca.

It's a list of features that will be cool in Emuteca:

General
-------
  * Fast as the speed of light or more.
  * Small as single bit or less.
  * Portable to any other platform or OS.

Auto...
-------
  * Make many tedious operatons automatic.
  * Autoupdate program.
  * Autoupdate scripts.
  * Autoupdate/Autosearch game, emulator, system data... And localised.
  * Auto... everything else

Help
----
  * Write hints, context help and a user manual.
  * Explain better how systems will be handled.

Data
----
  * Fill more data in databases.
  * Make reading and writing faster:
    * Writing: Saving content straight (simulating a ini file) when not merging.
    * Reading: Looping on ini sections and then searching on game list.
    * Keep individual method, they will be required when merging, for example.

Scripts
-------
  * Make scripts for use on Groups, Games or general (GameManager).
  * Make one for importing MAME data, and other files.
  * ¡¡BUG!! when creating a TStringList...
    begin
      aStringList := TStringList.Create;
      aStringList.Free; <--- MEMORY LEAK
    end.

Structure
---------
  * MAME's like audit for systems.
  * Group systems: by company, year, generation *cought*, type [Console/Hadheld]
  * Group emulators: by supported systems, license, some cool features (TAS)
  * Generalise grouping.