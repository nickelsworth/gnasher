{$CODEPAGE CP850}
program Gnasher;

uses
   crt,dos;

{$V-}

(* SHAREWARE PROGRAM AND SOURCE CODE - NOT FOR PROFIT SALE -
   CONTRIBUTIONS WELCOME.

   This is a variation of the old PacMan game with some twists.
   The "monsters" search in your DIRECTION - and they can see
   through walls! They can even "smell" you through the exit holes at the
   sides! You have to figure out a strategy to finish a level before
   your oxygen is used up! (Trap the monsters in certain places of the
   maze). It is also best to keep the monsters "together".

   You can use the source code to learn about Pascal. May be compiled
   using Turbo Pascal 5.5, 6.0, 7.0.

   You are encouraged to make new innovations, e.g. better graphics
   of the moving figures. (Originally the program was written on a CP/M
   machine (Gemini) where the graphics were used for better graphics of
   the monsters and the "pacman").

   If you make some improvements, please send a copy!

   Hope you will enjoy it!

   Yours sincerely,

   J›rgen Fog,

   Aalstrup Software
   Aalstrupvej 34, DK-8300 Odder,
   Denmark.

   Tlph. Denmark +45 86 55 16 97
*)

const
     Demo         = false;
     VERSION = '24'; (* the version of this program, max. 2 digits *)

     Closed       = #219;  (* man (gnasher) with closed mouth *)
     EatRight     = #16;  (* -      -      "eating right", etc. *)
     EatUp        = #30;
     EatDown      = #31;
     EatLeft      = #17;
     DotChar      = #3;  (* Dot, or cookie, character *)
     MonsterChar  = #1;
     EatenChar    = #249;  (* The 'remains' after an eaten cookie *)
     WallChar1    = #176;  (* used in different rounds *)
     WallChar2    = #177;
     WallChar3    = #178;
     DeadManChar  = '+';
     w11=5; w12=0; (* wall colors: text/background *)
     w21=2; w22=4;
     w31=3; w32=6;

type
     Maze = (Wall,Dot,Eaten); (* Dot = Cookie *)
     HScore  = record        (* high score *)
         Name :    string(.23.);
         Score :   integer;
         RoundNo : integer;
     end;
     ListOfScores = array(.1..15.) of HScore;
     AnyString = string(.37.);
     ScoreArr = array(.1..4.) of ListOfScores;
     str39 = string(.39.);
var
     List :       ListOfScores;
     ScoreArray:  ScoreArr;
     Rfile :      File of ScoreArr;
     Coo : array(.0..39.) of array(.0..24.) of Maze; (* coordinates *)
     I,O,P : integer;
     Move, MoX, MoY, MoXN, MoYN : array(.1..4.) of integer;  (* MoX etc.
        are monster coordinates. MOXN etc. are New coordinates (after a
        possible move *)
     MX,MY,OX,OY,KX,KY : integer;  (* Man-coordinates : M is Man, O is Old
        man (last position), K is the position which the man will get if
        he goes in the direction which is pressed on a KEY *)
     Points, RoundNo, Niveau, Oxy, (* oxygen reserves *)
        co (* count *), mo (* modulus *)  : integer;
     C, ch, Option : char;
     KeyDir, MoveDir: integer;  (* KeyDir = the keyed-in direction,
                                  MoveDir = the present actual moving
                                           direction *)
     Dead, slow : boolean; (* slow true if oxygen is used up *)
     StartOxy, EndOxy: integer; (* Oxygen reserves at start and end
                                   of a round *)
     DelayFactor: integer;
     NearHole: boolean;
     MinProb: real;  (* Minimum probability factor
                        (in SeekObject procedure) *)
     Level: integer; (* there are 3 levels with each their list of records *)
     OrigMode: integer;
     // reg: registers;
     copyrightstr1, copyrightstr2: str39;
     SoundOn: boolean;
     WallChar: char;

     (* colour variables: text/background *)
     w1,w2,   (* wall *)
     d1,d2,   (* dot *)
     m1,m2,   (* monster*)
     ma1,ma2, (* man *)
     t1,t2    (* text *)
           : byte;

(* 'STANDARD ROUTINES' : *)

const
  BS =   ^H; (* backspace *)
  CR =   ^M; (* carriage return *)
  ESC =  ^[; (* escape *)
  inv =  '';  (* alternative char.set, often = inverse *)
  norm = '';  (* normal char.set *)


procedure cursoff;
begin
  //reg.ax := $0100;
  //reg.cx := $2000;
  //intr($10,reg);
  crt.cursoroff;
end;

procedure curson;
begin
  //reg.ax := $0100;
  //reg.cx := $0007;
  //intr($10,reg);
  crt.cursoron;
end;


procedure Col(t, b: byte);
begin
  textcolor(t); textbackground(b);
end;

procedure RV; (* revers *)
begin
  col(t2,t1);
end;

procedure NRM; (* normal *)
begin
  col(t1,t2);
end;

procedure BlinkRV;
begin
   col(t2+blink, t1);
end;

procedure mc; (* man colour *)
begin col(ma1,ma2); end;

procedure SoundWon;
var x,y: integer;
begin
  if SoundOn then begin
    x:=random(780) + 20;
    y:=random(30) + 20;
    sound(x);   delay(y); nosound; delay(3*y);
    sound(x);   delay(y); nosound; delay(3*y);
    sound(2*x);  delay(y); nosound; delay(3*y);
    sound(2*x);  delay(y); nosound; delay(3*y);
    sound(6*x);  delay(2*y); nosound; delay(6*y);
    sound(8*x);  delay(2*y); nosound; delay(6*y);
    sound(10*x); delay(2*y); nosound; delay(6*y);
  end;
end;

procedure finishcopyright; forward;

procedure CheckCopyrightMsg;
var i: integer; a: longint;
begin
{
    a:=0;
    for i:=1 to length(copyrightstr1) do
      a:=a+ 1503-ord(copyrightstr1(.i.));
    for i:=1 to length(copyrightstr2) do
      a:=a+ 2789-ord(copyrightstr2(.i.));
     if a <> 139555 then begin ClrScr; FinishCopyright; end;

     THIS HAS BEEN DISABLED AS THE SOURCE CODE IS NOW SUPPLIED
     BUT SHOWS HOW YOU CAN PROTECT YOUR OWN PROGRAMS FROM BEING
     STRIPPED OF THE COPYRIGHT NOTICE
}
end;

procedure InitialiseRecords;
var ch: char; filename: string(.14.);
begin
    ClrScr;
    gotoxy(1,8);
    writeln('Gnasher -');
    writeln;
    writeln('Do you really want to initialise');
    writeln('records, i.e. put all the records to');
    writeln('200 and delete old records (if any) ?');
    write  ('(Y/N) : ');
    CursOn;
    repeat ch:=readkey; ch := upcase(ch); until (ch in (.'Y','N'.));
    writeln(ch); CursOff; writeln;
    clrscr;
    if ch='Y' then begin
       for i := 1 to 15 do begin
          with List(.i.) do begin
             Score   := 200;
             Name    := '';
             RoundNo := 1;
          end;
       end;
       for i := 1 to 4 do    ScoreArray(.i.) := List;
       filename := 'GNASH' + version + '.DAT';
       assign(Rfile, filename); rewrite(Rfile);
       (*$I- *)  write(Rfile, ScoreArray); (*$I+ *)
       if (IOResult = $F0) then begin
          ClrScr; gotoxy(1,10);
          writeln('The disk is full, so I cannot create a new datafile');
          writeln('- please make space or use a new disk.');
          writeln;
          halt;
       end;
       writeln('Records initialised!'); writeln;
    end else begin
      writeln('Records NOT initialised');
      writeln;
    end;
    write('Press ', inv, ' ENTER ', norm, ' ');
    repeat ch:=readkey until ch=CR;
    clrscr;
end;

Function UpC(ch: char): char; (* UpCase including Danish letters ‘›† *)
begin
  case ch of
    'a'..'z': UpC := upcase(ch);
    '‘': UpC := '’';
    '›': UpC := '';
    '†': UpC := '';
    else UpC := ch;
  end;
end;

procedure Msg (S: anystring);   (* Message *)
  begin
    gotoxy(1,23); clreol;
    gotoxy(1,24); clreol; RV; write(' ',s,' '); NRM;
  end;

Procedure WritePrompt;
begin
  gotoxy(1,23); ClrEol;
  gotoxy(1,22); ClrEol;
  RV; write(' -> '); NRM; write('  ');
end;

Procedure EnterText (PromptText: Anystring;
                    var Line:   Anystring;
                    MaxLength:  byte);
var    ch: char; functionKey: boolean;
begin
    curson;
    Msg(PromptText);  WritePrompt;
    gotoxy(MaxLength+7, 22);  write('/'); gotoxy(7,22);
    Line := '';
    repeat
      repeat
        functionkey:=false;
        ch:=readkey;
        case ch of
          #0: begin ch:=readkey; functionkey:=true; end;
          ' '..'}','‘','›','†','’','','':   begin
                         write(ch);  Line := Line+ch;
                      end;
          BS:         begin
                        if length(Line)>0 then begin
                          write(BS,' ',BS);  delete(Line,Length(Line),1);
                        end;
                      end;
          else        (* nothing *)
        end; (* case *)
      until (not FunctionKey) and ((ch=CR) or (length(Line)=MaxLength));
      if (length(Line)=MaxLength) then begin
        repeat  ch:=readkey;  until (ch in (.CR, BS.))
      end;
      if ch=BS then begin
        write(BS,' ',BS);  delete(Line,Length(Line),1);
      end;
    until ch=CR;
    cursoff;
    (* Now delete all spaces at beginning and end *)
    while (length(Line)>0) and (Line(.1.)=' ') do
       delete(Line, 1, 1);
    while (length(Line)>0) and (Line(.length(Line).)=' ') do
       delete(Line, length(Line), 1);
end;

procedure Gotoxy2(x, y: byte);
begin    gotoxy(x+1, y+1);  end;
(* as Turbo Pascal has home = 1,1, not 0,0 *)

procedure Finish;
begin
   ClrScr;
   seek(Rfile, 0); write(Rfile, ScoreArray);
   close(Rfile);
   gotoxy(1,5);
   writeln('- Thanks for the game.'); writeln; writeln;
   textmode(origmode); curson;
   halt;
end;

procedure FinishCopyright;
begin
   curson;
   halt;
end;

procedure DefineMaze;
begin
    for I := 0 to 39 do begin
        for O := 0 to 24 do begin
            Coo(.I,O.) := Wall;
        end;
    end;
    for I :=  1 to  5 do Coo(.I,1.) := Dot;
    for I :=  7 to 32 do Coo(.I,1.) := Dot;
    for I := 34 to 38 do Coo(.I,1.) := Dot;
    Coo(.1,2.) := Dot;   Coo(.5,2.) := Dot; Coo(.7,2.) := Dot;
    Coo(.19,2.) := Dot;
    Coo(.20,2.) := Dot; Coo(.32,2.) := Dot;
    for I := 34 to 38 do Coo(.I,2.) := Dot;
    Coo(. 1,3.) := Dot; Coo(.5,3.) := Dot; Coo(.7,3.) := Dot;
    Coo(.19,3.) := Dot;
    Coo(.20,3.) := Dot; Coo(.32,3.) := Dot; Coo(.34,3.) := Dot;
    Coo(.38,3.) := Dot;
    for I :=  0 to  5 do Coo(.I,4.) := Dot;
    for I := 10 to 29 do Coo(.I,4.) := Dot;
    Coo(. 7,4.) := Dot; Coo(.32,4.) := Dot; Coo(.34,4.) := Dot;
    Coo(.38,4.) := Dot;
    Coo(.39,4.) := Dot;
    Coo(. 1,5.) := Dot; Coo(.5,5.) := Dot; Coo(.7,5.) := Dot;
    Coo(.10,5.) := Dot;
    Coo(.19,5.) := Dot; Coo(.20,5.) := Dot; Coo(.29,5.) := Dot;
    Coo(.32,5.) := Dot;
    Coo(.34,5.) := Dot; Coo(.38,5.) := Dot;
    Coo(. 1,6.) := Dot; Coo(.5,6.) := Dot; Coo(.7,6.) := Dot;
    Coo(.10,6.) := Dot;
    Coo(.29,6.) := Dot; Coo(.32,6.) := Dot; Coo(.34,6.) := Dot;
    Coo(.38,6.) := Dot;
    for I := 11 to 28 do Coo(.I,6.) := Dot;
    Coo(. 1,7.) := Dot; Coo(.5,7.) := Dot; Coo(.7,7.) := Dot;
    Coo(.10,7.) := Dot;
    Coo(.29,7.) := Dot; Coo(.32,7.) := Dot; Coo(.34,7.) := Dot;
    Coo(.38,7.) := Dot;
    Coo(. 1,8.) := Dot; Coo(. 5,8.) := Dot; Coo(. 7,8.) := Dot;
    Coo(.10,8.) := Dot;

    Coo(.29,8.) := Dot; Coo(.32,8.) := Dot; Coo(.34,8.) := Dot;
    Coo(.38,8.) := Dot;
    Coo(. 6,8.) := Dot; Coo(.33,8.) := Dot;
    for I := 1 to 7 do Coo(.I,9.) := Dot;
    for I := 11 to 28 do Coo(.I,9.) := Dot;
    Coo(.10, 9.) := Dot; Coo(.29,9.) := Dot; Coo(.32,9.) := Dot;
    Coo(. 1,10.) := Dot; Coo(.7,10.) := Dot; Coo(.10,10.) := Dot;
    Coo(.19,10.) := Dot;
    Coo(.20,10.) := Dot; Coo(.29,10.) := Dot;
    for I := 32 to 38 do Coo(.I,10.) := Dot;
    Coo(.1,11.) := Dot; Coo(.7,11.) := Dot; Coo(.32,11.) := Dot;
    for I := 10 to 29 do Coo(.I,11.) := Dot;
    Coo(. 1,12.) := Dot; Coo(.7,12.) := Dot; Coo(.19,12.) := Dot;
    Coo(.20,12.) := Dot; Coo(.32,12.) := Dot;
    for I := 1 to 7 do Coo(.I,13.) := Dot;
    Coo(.19,13.) := Dot; Coo(.20,13.) := Dot; Coo(.32,13.) := Dot;
    Coo(. 1,14.) := Dot;
    for I := 5 to 38 do Coo(.I,14.) := Dot;
    Coo(. 1,15.) := Dot; Coo(.5,15.) := Dot; Coo(.22,15.) := Dot;
    Coo(.23,15.) := Dot;
    Coo(.38,15.) := Dot; Coo(.1,16.) := Dot;
    for I := 5 to 9 do Coo(.I,16.) := Dot;
    for I := 11 to 14 do Coo(.I,16.) := Dot;
    for I := 16 to 38 do Coo(.I,16.) := Dot;
    Coo(. 1,17.) := Dot; Coo(.5,17.) := Dot; Coo(.7,17.) := Dot;
    Coo(. 9,17.) := Dot;
    Coo(.16,17.) := Dot; Coo(.18,17.) := Dot; Coo(.27,17.) := Dot;
    Coo(.29,17.) := Dot; Coo(.36,17.) := Dot; Coo(.38,17.) := Dot;
    for I := 11 to 14 do Coo(.I,17.) := Dot;
    for I := 20 to 25 do Coo(.I,17.) := Dot;
    Coo(. 1,18.) := Dot; Coo(.5,18.) := Dot; Coo(.7,18.) := Dot;
    Coo(. 9,18.) := Dot;
    Coo(.11,18.) := Dot; Coo(.14,18.) := Dot; Coo(.16,18.) := Dot;
    Coo(.18,18.) := Dot; Coo(.20,18.) := Dot; Coo(.25,18.) := Dot;
    Coo(.27,18.) := Dot; Coo(.29,18.) := Dot; Coo(.36,18.) := Dot;
    Coo(.38,18.) := Dot;
    Coo(. 7,19.) := Dot; Coo(.9,19.) := Dot;
    Coo(.11,19.) := Dot; Coo(.14,19.) := Dot; Coo(.16,19.) := Dot;
    Coo(.18,19.) := Dot; Coo(.20,19.) := Dot; Coo(.25,19.) := Dot;
    Coo(.27,19.) := Dot; Coo(.29,19.) := Dot; Coo(.36,19.) := Dot;
    Coo(.38,19.) := Dot;
    for I := 1 to 5 do   Coo(. I,19.) := Dot;
    for I := 0 to 5 do   Coo(. I,20.) := Dot;
    Coo(. 7,20.) := Dot; Coo(. 9,20.) := Dot;
    Coo(.11,20.) := Dot; Coo(.14,20.) := Dot; Coo(.16,20.) := Dot;
    Coo(.18,20.) := Dot; Coo(.20,20.) := Dot; Coo(.25,20.) := Dot;
    Coo(.27,20.) := Dot; Coo(.29,20.) := Dot; Coo(.36,20.) := Dot;
    Coo(.38,20.) := Dot; Coo(.10,20.) := Dot; Coo(.15,20.) := Dot;
    Coo(.39,20.) := Dot;
    Coo(. 1,21.) := Dot; Coo(. 5,21.) := Dot; Coo(. 7,21.) := Dot;
    Coo(.18,21.) := Dot; Coo(.27,21.) := Dot; Coo(.38,21.) := Dot;
    for I :=  9 to 11 do Coo(. I,21.) := Dot;
    for I := 14 to 16 do Coo(. I,21.) := Dot;
    for I := 20 to 25 do Coo(. I,21.) := Dot;
    for I := 29 to 36 do Coo(. I,21.) := Dot;
    Coo(. 1,22.) := Dot; Coo(. 5,22.) := Dot; Coo(. 7,22.) := Dot;
    Coo(.18,22.) := Dot; Coo(.27,22.) := Dot; Coo(.38,22.) := Dot;
    for I := 1 to  5 do  Coo(. I,23.) := Dot;
    for I := 7 to 38 do  Coo(. I,23.) := Dot;
end;

procedure WallWrite;
begin  Col(w1,w2); write(WallChar); nrm; end;

procedure DotWrite;
begin  Col(d1,d2); write(DotChar); nrm; end;

procedure MonsterWrite;
begin  Col(m1,m2); write(MonsterChar); nrm; end;

procedure DrawMaze;
begin
    case niveau of
       1: WallChar := WallChar1;
       2: WallChar := WallChar2;
       3: WallChar := WallChar3;
    end;
    case niveau of
       1: begin w1:=w11; w2:=w12; end;
       2: begin w1:=w21; w2:=w22; end;
       3: begin w1:=w31; w2:=w32; end;
    end;
    nrm; ClrScr;
    for I := 0 to 39 do begin
        for O := 0 to 23 do begin (* last line is a special case *)
            Gotoxy2(I,O);
            if Coo(.I,O.) = Wall then
              WallWrite
            else
              DotWrite;
        end;
    end;
    for I := 0 to 38 (* OBS as the bottom right corner must be empty
                        (otherwise the screen would scroll)  *)
    do begin
            Gotoxy2(I,24);
            if Coo(.I,24.) = Wall then WallWrite
            else                       DotWrite;
    end;
    RV;
    gotoxy2(1,0);  write(' POINTS : ', Points:0, ' ');
    gotoxy(14,8); write(Norm, ' P = Pause    ', inv);
    gotoxy(14,9); write(norm, ' S = Sound +/-');
    gotoxy2(20, 0); write(' HIGH SCORE:', List(.1.).Score : 6,' ');
    gotoxy2(30,17);             write(' Move:');
    gotoxy2(30,18);             write('   A  ');
    gotoxy2(30,19);             write(' ,   .');
    gotoxy2(30,20);             write('   Z  ');

    gotoxy2( 1,24);            write(' ROUND ', RoundNo);
                               write(' LEVEL ', level,' ');
    gotoxy2(18,24);            write(' OXYGEN:', Oxy:6, ' ');

    gotoxy2(33,24);            write(' V:', version,' ');
    NRM;
    Gotoxy2(MoX(.1.),MoY(.1.)); MonsterWrite;
    Gotoxy2(MoX(.2.),MoY(.2.)); MonsterWrite;
    Gotoxy2(MoX(.3.),MoY(.3.)); MonsterWrite;
    Gotoxy2(MoX(.4.),MoY(.4.)); MonsterWrite;
    repeat
      repeat
          gotoxy2(MX,MY); col(ma1,ma2); write(EatUp); delay(120);
          gotoxy2(9,23);  RV; write(' Press '); nrm; write(' S ');RV;
            write(' to start ');
          delay(120);
          gotoxy2(MX,MY); col(ma1,ma2); write(Closed); delay(120);
          gotoxy2(9,23);  NRM; write(' Press '); RV; write(' S '); NRM;
            write(' to start ');
          delay(120);
      until keypressed;
      ch:=readkey; ch := upcase(ch);
    until ch = 'S';
    gotoxy2(9,23); for i := 1 to 23 do write(DotChar);
    (* that was where the above message was blinking *)
end; (* DrawMaze *)

procedure Init_start;
var filename: string(.14.);
begin
    nrm; ClrScr;
    write('Preparing for Gnasher ... ');
    filename := 'GNASH' + version + '.DAT';
    (*$I- *) assign(Rfile, filename); reset(Rfile);  (*$I+ *)
    if IOResult<>0 then begin
       ClrScr; gotoxy(1,10);
       writeln('The datafile GNASH',version,'.DAT is not');
       writeln('on the default drive.');
       writeln;
       writeln('Do you want to create');
       write  ('a new file ? (Y/N) ');
       repeat ch:=readkey; ch:=upcase(ch); until (ch in (.'Y','N'.));
       if ch='Y' then InitialiseRecords
       else begin
          ClrScr;
          writeln; writeln;
          writeln('As you didn''t want me to make a new datafile,');
          writeln('I can''t run the Gnasher program.');
          writeln('If it was a mistake, just start again.');
          writeln;
          writeln('Else I hope to see you later!');
          writeln;
          writeln('Yours sincerely,');
          writeln;
          writeln('             The Gnasher');
          writeln;
          Halt;
       end;
    end;
    randomize;
    CursOff;  (* disable cursor *)
    seek(Rfile, 0);
    read(Rfile, ScoreArray);
    Level := 1;
    List := ScoreArray(.Level.); (* start on level 1, first time *)
end;  (* Init_start *)

procedure Explain;
begin
  ClrScr; writeln; writeln;
    writeln('GNASHER (version ', version, '):' );
    writeln('---------------------------------------');
    writeln('              <-- You');
    for i := 1 to 11 do write(DotChar);
    write(Norm); writeln('   Cookies');
    for i := 1 to 4 do begin
      write(inv); MonsterWrite; write(Norm); write(' ');
    end;
    writeln('      Monsters');
    writeln;
    writeln('Try to eat as many cookies as possible,');
    writeln('without being caught by the monsters.');
    writeln('Use the keys A and Z to go up and down,');
    writeln('and comma/period to go left and right -');
    writeln('just press a key once for change of');
    writeln('direction.');
    writeln;
    writeln('During the game you may use:'); writeln;
    writeln('ESC = Abort programme');
    writeln('P   = Pause');
    writeln('S   = Sound on/off');
    writeln;
    write('Press '); RV; write(' ENTER. '); NRM;
    repeat
      repeat
        mc; (* man color*)
        gotoxy(1,5); write(EatRight); delay(200);
        gotoxy(1,5); write(Closed);   delay(200);
      until keypressed;
      ch:=readkey;
    until ch=CR;
    nrm; ClrScr;
    writeln('NB: When you (or the monsters!) leave');
    writeln('the labyrint at one side, you will');
    writeln('enter in the opposite side. Take care!');
    writeln;
    writeln('You have only limited oxygen reserves');
    writeln('for each round!');
    writeln;
    writeln('After each round it becomes more');
    writeln('difficult.');
    writeln;
    writeln('Hint: try to catch the monsters in');
    writeln('"pockets" of the maze. The monsters');
    writeln('always go in your direction (except');
    writeln('near exits!)');
    writeln;
    write('Press '); RV;  write(' Space '); NRM;
    repeat ch:=readkey;  until ch = ' ';
end;

procedure ShowRecords;
begin
    ClrScr; RV;
    writeln( ' Gnasher : Heroes on Level ', level,' '); NRM;
    writeln;
    writeln('POINTS  ROUND   NAME');
    writeln('---------------------------------------');
    for I := 1 to 15 do
      writeln(List(.I.).Score:5, List(.I.).RoundNo:7,'    ',List(.I.).Name);
    writeln('---------------------------------------');
    writeln(inv, ' ENTER ',Norm,' = Start.  ',inv,' ESC ',Norm,' = Stop.');
    writeln(inv, ' H ', Norm, ' = Help.');
    writeln(inv, ' L ', Norm, ' = Change Level.');
    write(  inv, ' I ', Norm, ' = Initialise records.');
end;

procedure DecideLevel;
var  ch: char;  NewLevel: integer;
begin
   ClrScr; gotoxy(1,6);
   writeln('Gnasher Levels :');
   writeln('----------------');
   writeln;
   writeln('1  =  Beginner');
   writeln('2  =  Advanced');
   writeln('3  =  Expert');
   writeln('4  =  Master');
   writeln;
   writeln;
   write('Press Level : ',  inv,' 1 ',norm,' ',
                            inv,' 2 ',norm,' ',
                            inv,' 3 ',norm,' or ',
                            inv,' 4 ',norm);
   repeat  ch:=readkey; until (ch in (.'1','2','3','4',ESC .));
   if ch=ESC then Finish;
   NewLevel := ord(ch) - 48;
   if NewLevel <> Level then begin
      Level := NewLevel;
      List := ScoreArray(.Level.);
   end (* if *) ;
end;  (* DecideLevel *)

procedure Start;
begin
    Points := 0;
    RoundNo := 1;  EndOxy := 0;
end;

procedure InitRound;  (* initiations before a new round *)
begin
    dead := false; slow := false; nrm; clrscr;
    case level of
       1: StartOxy := 2100;
       2: StartOxy := 2000;
       3: StartOxy := 1900;
       4: StartOxy := 2200;
    end;
    Oxy := StartOxy + EndOxy;
    case level of
       1: if (Oxy > 2600) then Oxy := 2600;
       2: if (Oxy > 2500) then Oxy := 2500;
       3: if (Oxy > 2400) then Oxy := 2400;
       4: if (Oxy > 2500) then Oxy := 2500;
    end;

    case level of
       1: MinProb := 0.30;
       2: MinProb := 0.25;
       3: MinProb := 0.20;
       4: MinProb := 0.15;
    end;
    MinProb := MinProb - (RoundNo * 0.04);
    if MinProb < 0.1 then MinProb := 0.1;

    case level of
       1:  delayfactor := 35;
       2:  delayfactor := 22;
       3:  delayfactor := 16;
       4:  delayfactor := 10;
    end; (* case *)
    delayfactor := delayfactor - (RoundNo * 2);
    if delayfactor < 0 then delayfactor := 10;

    if (RoundNo mod 3 = 0) then Niveau := 3
       else if (RoundNo mod 3 = 1) then Niveau := 1
          else if (RoundNo mod 3 = 2) then Niveau := 2;
    case level of
       1:  (* nothing *) ;
       2: Niveau := Niveau + 1;
       3: Niveau := Niveau + 2;
       4: (* nothing *) ;
    end;
    if niveau = 4 then niveau := 1;
    if niveau = 5 then niveau := 2;

    co := 0; (* count *)
    MO := 4;  (* modulus factor, used with count *)
    KeyDir := 8;   MoveDir := 8;

    MX := 20; OX := 20; KX := 20;  (* initial man-coordinates *)
    MY := 14; OY := 14; KY := 14;

    MOVE(.1.) := 1;
    MOVE(.2.) := 1;
    MOVE(.3.) := 1;
    MOVE(.4.) := 1;

    MoXN(.1.) := 7;  (* initial monster coordinates *)
    MoYN(.1.) := 5;
    MoXN(.2.) := 32;
    MoYN(.2.) := 4;
    MoXN(.3.) := 5;
    MoYN(.3.) := 19;
    MoXN(.4.) := 37;
    MoYN(.4.) := 16;

    MoX(.1.) := 7;
    MoY(.1.) := 5;
    MoX(.2.) := 32;
    MoY(.2.) := 4;
    MoX(.3.) := 5;
    MoY(.3.) := 19;
    MoX(.4.) := 37;
    MoY(.4.) := 16;

end;  (* InitRound *)

procedure Slow_Man;
var ch: char;
begin
  nrm; ClrScr;
  writeln; writeln; writeln('You didn''t manage the round.'); writeln;
  writeln('Oxygen exhausted - sorry!'); writeln;
  writeln('You got ', Points, ' points.'); writeln;
  writeln('Press ', inv, ' ENTER. ', Norm);
  repeat  ch:=readkey; until ch=CR;
end;

procedure Dead_Man;
var i,j: integer;
begin
    write(inv);
    gotoxy2(mx,my); BlinkRV; write(DeadManChar);
    if SoundOn then begin
      for j:=1 to 1 do begin     (* evt 1 to 2 *)
         for i:= 720 downto 450 do begin
           sound(i); delay(3);
         end;
         nosound; delay(100);
      end;
      delay(1000);
    end
    else delay(2000);
    Nrm;
    ClrScr;
    gotoxy(1,6);
    writeln('- - - ALAS !! - - -');
    writeln;
    writeln('- you have been devoured by a');
    writeln('greedy monster!');
    writeln;
    writeln('You got ', Points, ' points');
    writeln('and had oxygen reserves ', Oxy );
    writeln('before you expired in round no. ', RoundNo);
    writeln('on level ', level,'.');
    writeln;
    writeln('Better luck next time!');
    writeln;
    writeln('Press ', inv, ' ENTER. ', Norm);
    repeat  ch:=readkey; until ch=CR;
end;

procedure Replay;
begin
    SoundWon;
    EndOxy := Oxy;
    ClrScr;
    Gotoxy2(0,8);
    writeln('You managed ',RoundNo,'. round'); writeln;
    writeln('Congratulations!'); writeln;
    writeln('Oxygen reserves at the end were: ', EndOxy);
    writeln('which are transferred to the next round');
    writeln('(but you can''t have more than a');
    writeln('certain maximum of oxygen).');
    writeln;
    writeln('Now you have ', Points, ' points.'); writeln;
    writeln;
    writeln('Press ',inv, ' ENTER ', Norm, ' to proceed.');
    repeat
         c:=readkey;
    until c = CR;
end;

procedure MoveBack; (* moves the monsters back if Move_a_Monster has moved
                       them into a Wall - or into another Monster ! *)
begin
   case Move(.I.) of
      1:  MoXN(.I.) := MoXN(.I.) - 1;
      2:  MoXN(.I.) := MoXN(.I.) + 1;
      3:  MoYN(.I.) := MoYN(.I.) - 1;
      4:  MoYN(.I.) := MoYN(.I.) + 1;
   end;
end;  (* MoveBack *)

procedure Move_a_Monster;   (* moves monsters according to Move *)
 (* Move is like this:          4
                              2   1
                                3            *)
begin
if (Move(.I.) = 1) and ((MoXN(.I.) < 38) or (MoYN(.I.) in (.4,20.) ))
   then MoXN(.I.) := MoXN(.I.) + 1;
if MoXN(.I.) = 40 then MoXN(.I.) := 0;  (* i.e. a hole *)

if (Move(.I.) = 2) and ((MoXN(.I.) > 1) or (MoYN(.I.) in (.4,20.) ))
   then MoXN(.I.) := MoXN(.I.) - 1;
if MoXN(.I.) = -1 then MoXN(.I.) := 39;  (* i.e. a hole *)

if (Move(.I.) = 3) and (MoYN(.I.) < 23) then MoYN(.I.) := MoYN(.I.) + 1;
if (Move(.I.) = 4) and (MoYN(.I.) >  1) then MoYN(.I.) := MoYN(.I.) - 1;
end; (* Move_a_Monster *)

procedure WriteOxy;
begin
  gotoxy2(26,24); RV;
  write(Oxy:6); NRM;
end;


procedure OpenMouthK;
(* open the mouth on the man when moved according to KX,KY  *)
begin
     case MoveDir of (* MoveDir is then = KeyDir *)
       4: begin Gotoxy2(KX,KY); mc; write(EatLeft); nrm; end;
       6: begin Gotoxy2(KX,KY); mc; write(EatRight); nrm; end;
       8: begin Gotoxy2(KX,KY); mc; write(EatUp); nrm; end;
       2: begin Gotoxy2(KX,KY); mc; write(EatDown); nrm;end;
     end (* case *) ;
end;

procedure OpenMouthM;
(* open the mouth on the man when moved according to MX,MY   *)
begin
     case MoveDir of
       4: begin Gotoxy2(MX,MY); mc; write(EatLeft); nrm; end;
       6: begin Gotoxy2(MX,MY); mc; write(EatRight); nrm; end;
       8: begin Gotoxy2(MX,MY); mc; write(EatUp); nrm; end;
       2: begin Gotoxy2(MX,MY); mc; write(EatDown); nrm; end;
     end (* case *) ;
end;

procedure MoveMouthM;
begin
   if (co mod MO) > (MO div 2) then begin
      Gotoxy2(MX,MY); mc; write(Closed); nrm;
   end
   else OpenMouthM;
end;

procedure MoveMouthK;
begin
   if (co mod MO) > (MO div 2) then begin
      Gotoxy2(KX,KY); mc; write(Closed); nrm;
   end
   else OpenMouthK;
end;

procedure SeekObject ( fromX, fromY, ToX, ToY : integer);
   (* general procedure for monsters to seek objects (holes or
   Man, by defining Move(.i.) which is the direction.
   The x/y relation is taken into account when defining
   Probability (of taken xdirection instead of ydirection) *)

var  xdiff, ydiff, Probab, XYrelation: real;
      (* MinProb is the minimum accepted probability *)
begin
       xdiff := tox - fromx; if xdiff=0 then xdiff := 0.1;
       ydiff := toy - fromy; if ydiff=0 then ydiff := 0.1;
       XYrelation := abs(xdiff/ydiff);
       if XYrelation < 1 then XYrelation := 1/XYrelation;
       Probab := 1 / (1 + XYrelation);
       if Probab < MinProb then Probab := MinProb;

       if random > Probab then begin  (* i.e. this is more probable *)
          if (abs(xdiff) >= abs(ydiff)) then begin
             if (xdiff >= 0) then Move(.I.) := 1 else Move(.I.) := 2;
          end else begin
             if (ydiff >= 0) then Move(.I.) := 3 else Move(.I.) := 4;
          end;
       end else begin  (* this is less likely *)
          if (abs(xdiff) <  abs(ydiff)) then begin  (* NB the < *)
             if (xdiff >= 0) then Move(.I.) := 1 else Move(.I.) := 2;
          end else begin
             if (ydiff >= 0) then Move(.I.) := 3 else Move(.I.) := 4;
          end;
       end;
end;  (* SeekObject *)

procedure CheckHole;  (* Checks whether the monster is near a hole and the
  man at the same time is near the wall on the other side. If this is the
  case, the monster will seek the hole *)

var  xdiff, ydiff,
     Quadrant  (* i.e. where the monster is *)  : integer;
     (* like this:   4  1
                     3  2    *)

const
   MINX =  9;  MINY =  9;  (* these values indicate the limits *)
   MAXX = 31;  MAXY = 15;  (* inside which NearHole is true *)

begin
   Quadrant := 0;
   if (MX < minx) and (MoXn(.i.) > maxx) then begin
      if MoYn(.i.) < miny then Quadrant := 1;
      if MoYn(.i.) > maxy then Quadrant := 2;
   end else begin
   if (MX > maxx) and (MoXn(.i.) < minx) then begin
      if MoYn(.i.) < miny then Quadrant := 4;
      if MoYn(.i.) > maxy then Quadrant := 3;
      end;
   end;
   if Quadrant = 0 then Nearhole := false else Nearhole := true;
   if Nearhole then begin
      case Quadrant of
         1: SeekObject( MoXn(.i.), MoYn(.i.), 40,  4);
         2: SeekObject( MoXn(.i.), MoYn(.i.), 40, 20);
         3: SeekObject( MoXn(.i.), MoYn(.i.), -1, 20);
         4: SeekObject( MoXn(.i.), MoYn(.i.), -1,  4);
      end;
   end;
end;  (* CheckHole *)

procedure MoveMonsters;
var AnotherM: boolean;  t: shortint;
begin
   Oxy  := Oxy-1;   if Oxy<=0 then slow := true;  WriteOxy;
   co := co+1; if co > 32000 then co := 0;
   MoveMouthM;
      for I := 1 to 4 do begin
        CheckHole;
        if not Nearhole then
            Seekobject(MoXn(.i.), MoYn(.i.), MX, MY);

        Move_a_Monster;  (* moves monsters according to Move *)
      {
        AnotherM:=false;
        for t:=1 to 4 do begin (* check if another monster is there*)
          if (I<>t) and
          ( Coo(.MoXN(.I.),MoYN(.I.).) = Coo(.MoXn(.t.),MoYn(.t.).) )
          then AnotherM:=true;
        end;
      }
        if (Coo(.MoXN(.I.),MoYN(.I.).) =  Wall)
      { or AnotherM }  then MoveBack

           (* MoveBack moves monsters back if they moved into a wall -
              or another monster! *)

        else begin
           Gotoxy2(MoX(.I.),MoY(.I.));
           if Coo(.MoX(.I.),MoY(.I.).) = Dot then write(DotChar);
           if Coo(.MoX(.I.),MoY(.I.).) = Eaten then write(EatenChar);
           Gotoxy2(MoXN(.I.),MoYN(.I.));
           MonsterWrite;
           MoX(.I.) := MoXN(.I.);
           MoY(.I.) := MoYN(.I.);
           if (MX = MoX(.I.)) and (MY = MoY(.I.)) then Dead := true;
        end;
        delay(DelayFactor);
      end;
end; (* MoveMonsters *)

procedure MoveMoveDir; (* i.e. Move in direction of MoveDir, NOT
                         as per KeyDir *)
label Exit;
begin
           case MoveDir of
              4:   MX := MX - 1;
              6:   MX := MX + 1;
              8:   MY := MY - 1;
              2:   MY := MY + 1;
           end;
           if MX = 40 then MX :=  0;  (* i.e. a hole *)
           if MX = -1 then MX := 39;
           for I := 1 to 4 do begin
               if (MoX(.I.) = MX) and (MoY(.I.) = MY) then begin
                   Dead := true;
                   gotoxy2(OX,OY);
                   if Coo(.OX,OY.) = Dot then
                      write(DotChar)
                   else
                   if Coo(.OX,OY.) = Eaten then
                      write(EatenChar);
                   goto Exit;
               end;
           end;
           if Coo(.MX,MY.) = Dot then begin
               Points := Points + 1;
               if (Points >= 32000) then Points := 0;
               Gotoxy2(11,0); RV; write(Points,' '); NRM;
               Coo(.MX,MY.) := Eaten;
               Gotoxy2(OX,OY); write(EatenChar);
               MoveMouthM;
               OX := MX;
               OY := MY;
           end else begin
               if Coo(.MX,MY.) = Eaten then begin
                   Gotoxy2(OX,OY); write(EatenChar);
                   MoveMouthM;
                   OX := MX;
                   OY := MY;
                end
                else begin
                   if Coo(.MX,MY.) = Wall then begin
                      MX := OX;
                      MY := OY;
                   end;
                end;
           end;
           KX := MX;
           KY := MY;
           Exit:
end; (* MoveMoveDir *)

procedure SoundEatenCake;
var i: integer;
begin
  {
  if soundOn then begin  i:=200;
    repeat
      sound(i); delay(12); inc(i,80);
    until i>450;
    nosound;
  end;
  }
  if soundON then begin
     sound(200); delay(12); nosound;
     sound(283); delay(12); nosound;
     sound(400); delay(12); nosound;
     {sound(566); delay(12); nosound;}
  end;
end;

procedure MoveSound;
begin
  if soundon then begin
      sound(200); delay(1); nosound;
      sound(252); delay(1); nosound;
      sound(300); delay(1); nosound;
  end;
end;

procedure MoveMan;
var functionKey: boolean;
label Exit;
begin
    co := co+1; (* count, used to open/close mouth of man *)
    if keypressed then begin
        functionkey:=false;
        c:=readkey;  c := Upc(C);
        case C of
          #0: begin
                c:=readkey; functionkey:=true;
              end;
           ESC, ^C :  Finish;
           'S'     :  SoundOn := not SoundOn;
          'P',^S   :  begin (* pause! *)
                          ch:=readkey;
                      end;
          'A','Z', '.', ',' :  begin
                                  co := 0;
                                  case c of
                                     ',' : KeyDir := 4;
                                     '.' : KeyDir := 6;
                                     'A' : KeyDir := 8;
                                     'Z' : KeyDir := 2;
                                  end; (* case *)
                                end;
        end;  (* case *)
     end; (* if keypressed *)

     (* Now check if it is possible to Move in last pressed direction
        (KeyDir) and do so if possible, else Move according to ongoing
        Movedirection (MoveDir), if possible *)

        case KeyDir of
           4: KX := MX-1;
           6: KX := MX+1;
           8: KY := MY-1;
           2: KY := MY+1;
        end (* case *) ;

        if KX = 40 then KX :=  0;  (* when moving through holes *)
        if KX = -1 then KX := 39;

        for I := 1 to 4 do begin
            if (MoX(.I.) = KX) and (MoY(.I.) = KY) then begin
                Dead := true;
                gotoxy2(OX,OY);
                if Coo(.OX,OY.) = Dot then
                   write(DotChar)
                else
                if Coo(.OX,OY.) = Eaten then
                   write(EatenChar);
                MX := KX;  MY := KY;
                goto Exit;
            end;
        end;
        if Coo(.KX,KY.) = Dot then begin
            SoundEatenCake;
            MoveDir := KeyDir; (* OBS *)
            Points := Points + 1;
            if (Points >= 32760) then Points := 0; (* just in case!!
               - if it happens there will be other troubles, but it will
               take 75 rounds to get to this number!! *)

            Gotoxy2(11,0); RV; write(Points,' '); NRM;
            Coo(.KX,KY.) := Eaten;
            Gotoxy2(OX,OY); write(EatenChar);
            MoveMouthK;
            MX := KX;   OX := KX;
            MY := KY;   OY := KY;
        end else begin
            MoveSound;
            if Coo(.KX,KY.) = Eaten then begin
                MoveDir := KeyDir; (* OBS *)
                Gotoxy2(OX,OY); write(EatenChar);
                MoveMouthK;
                MX := KX;   OX := KX;
                MY := KY;   OY := KY;
            end else begin
                if Coo(.KX,KY.) = Wall then
                MoveMoveDir; (* Move according to MoveDir, NOT KeyDir *)
            end;
        end;
    delay(DelayFactor);
    Exit:
end; (* MoveMan *)

procedure High;
label Exit;
begin
    for I := 1 to 15 do begin
        if Points > List(.I.).Score then begin
            ClrScr; gotoxy(1,6);
            writeln('- but though you died, your heroic');
            writeln('deeds will be remembered by future');
            writeln('generations.');
            writeln;
            writeln('Please write you name below.');
            writeln;
            writeln('(Use ',inv, ' Back Space ', norm, ' to correct errors,');
            writeln('and ', inv, ' ENTER ', norm, ' when the name is');
            writeln('correct).');
            for O := 15 downto I do begin
                List(.O.).Score   := List(.O-1.).Score;
                List(.O.).Name    := List(.O-1.).Name;
                List(.O.).RoundNo := List(.O-1.).RoundNo;
            end;
            List(.I.).Name := '';
            repeat
               EnterText('Name ?',  List(.I.).Name,  23);
            until List(.I.).Name <> '';

            List(.I.).Score := Points;    List(.I.).RoundNo := RoundNo;
            ScoreArray(.Level.) := List;
            goto Exit;
        end;
    end;
    Exit:
end;

procedure InitGrafik;
begin
    origmode:=lastmode;
    if (origmode=mono)
      {or true (* for testing mono *)}
      then begin
      w1 :=7; w2 :=0;
      d1 :=7; d2 :=0;
      m1 :=7; m2 :=0;
      ma1:=7; ma2:=0;
      t1 :=7; t2 :=0;
    end else begin
      w1 :=3; w2 :=6;
      d1 :=7; d2 :=1;
      m1 :=0; m2 :=4;
      ma1:=2; ma2:=5;
      t1 :=7; t2 :=1;
    end;
    if origmode <> mono then begin
      if (origmode in (.0..3.)) then textmode(c40);
    end;
end;

(* ----------------------- M A I N --------------------------------- *)

begin
    SoundON:=true;
    InitGrafik;
    Init_start; (* initial start *)
    copyrightstr1:=
      'Copyright by J›rgen Fog, lstrupvej 34,';
    copyrightstr2:=
      'DK-8300 Odder.';
{    checkcopyrightmsg;
     -  disabled now - but shows how you can protect
     your programs from being stripped of their copyright notice)
}
    CheckBreak:=false;
    cursoff;
    gotoxy(1,6); rv;
    writeln(' G N A S H E R '); nrm;
    WRITELN;
    writeln(CopyrightStr1);
    writeln('DK-8300 Odder, Denmark');
{    writeln(CopyrightStr2);}
    writeln;
    writeln;
    write('Press '); rv; write(' Esc '); nrm;
    repeat ch:=readkey; until ch=ESC;
    repeat
        Start; (* Start in the beginning, and after you died *)
        repeat
           ShowRecords;
           repeat
               option:=readkey;; Option := upcase(Option);
           until (Option in (.ESC,CR,'H','L','I'.));
           case Option of
               ESC: Finish;
               'H': Explain;
               'L': DecideLevel;
               'I': InitialiseRecords;
               CR: ;
           end  (* case *) ;
        until Option = CR;
        repeat
            InitRound; (* initialisations before a round *)
            DefineMaze;
            DrawMaze;
            repeat
                if not Dead then MoveMan;
                if not Dead then MoveMonsters;
            until (Points = 433 * RoundNo) or Dead or slow;

            if Dead then Dead_Man
                else if slow then slow_Man
                else Replay;
            if Dead or slow then High;  (* check if you come on
                                           high score list *)
            RoundNo := RoundNo + 1;
            if (roundNo>2) and demo then begin
              clrscr; gotoxy(1,12); Writeln('This is a demo.');
              writeln('You can''t go beyond 2nd round.');
              writeln('Please pay 90 kr. to the author');
              writeln('to be able to go beyond to further');
              writeln('exciting levels. Thank you.');
              writeln('Press Enter');
              repeat ch:=readkey; until ch=CR;
              Finish;halt; halt;
            end;
        until Dead or slow;
   until 1 = 2;
end.

(* end of file G.pas *)
