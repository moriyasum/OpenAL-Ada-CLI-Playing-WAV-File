---------------------------------------------------------
--  Playing WAV file with OpenAL and Ada
--  Ada application
--  Playback WAV file (with JUNK chunk support)
--  File size should be less than 7.8MByte
---------------------------------------------------------

with Ada.Text_IO;     use Ada.Text_IO;
with Interfaces;      use Interfaces;
with OpenAL.Context;  use OpenAL.Context;
with OpenAL.Buffer;   use OpenAL.Buffer;
with OpenAL.Source;   use OpenAL.Source;
with OpenAL.Thin;     --  use OpenAL.Thin;
with OpenAL.Types;    use OpenAL.Types;
with Ada.Unchecked_Deallocation;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Streams;     use Ada.Streams;

procedure Play_Wav  is
   FILE_NAME : constant String := "run-1ch-44.1k-16bit-33s.wav";
--  NOTE: the WAV file size limitation is between 7.8MB and 9.4MB
--  It will cause Memory error when the file size exceeds it.

   --  WAV file structure
   type WAV_Header is record
      ChunkID       : String (1 .. 4);
      ChunkSize     : Unsigned_32;
      Format        : String (1 .. 4);
      Subchunk1ID   : String (1 .. 4);
      Subchunk1Size : Unsigned_32;
      AudioFormat   : Unsigned_16;
      NumChannels   : Unsigned_16;
      SampleRate    : Unsigned_32;
      ByteRate      : Unsigned_32;
      BlockAlign    : Unsigned_16;
      BitsPerSample : Unsigned_16;
      Subchunk2ID   : String (1 .. 4);
      Subchunk2Size : Unsigned_32;
   end record;

   --  8bit for reading Header
--   type Byte is new Unsigned_8;
--   type Byte_Array is array (Positive range <>) of Byte;
--   type Byte_Array_Access is access all Byte_Array;

   --  16bit for Audio data
   type Sample_16_t_Array_Access is access all Sample_Array_16_t;
   --  16bit WAV store type

   type WAV_Data_Record is record
      Header : WAV_Header;
      Data   : Sample_16_t_Array_Access;
      Sample_Count : Natural;           -- Number of 16-bit samples
   end record;

   procedure Free is new Ada.Unchecked_Deallocation
     (Sample_Array_16_t, Sample_16_t_Array_Access);

   --  Helper function to read 4 bytes as string
   function Read_Chunk_ID
     (BinF : in out Ada.Streams.Stream_IO.File_Type) return String is
      Buffer : Ada.Streams.Stream_Element_Array (1 .. 4);
      Last : Ada.Streams.Stream_Element_Offset;
   begin
      Ada.Streams.Stream_IO.Read (BinF, Buffer, Last);
      return Character'Val (Buffer (1)) &
             Character'Val (Buffer (2)) &
             Character'Val (Buffer (3)) &
             Character'Val (Buffer (4));
   end Read_Chunk_ID;

   --  Helper function to read 4 bytes as Unsigned_32 (little endian)
   function Read_Uint32
     (BinF : in out Ada.Streams.Stream_IO.File_Type) return Unsigned_32 is
      Buffer : Ada.Streams.Stream_Element_Array (1 .. 4);
      Last : Ada.Streams.Stream_Element_Offset;
   begin
      Ada.Streams.Stream_IO.Read (BinF, Buffer, Last);
      return Unsigned_32 (Buffer (1)) +
             Unsigned_32 (Buffer (2)) * 2**8 +
             Unsigned_32 (Buffer (3)) * 2**16 +
             Unsigned_32 (Buffer (4)) * 2**24;
   end Read_Uint32;

   --  Helper function to read 2 bytes as Unsigned_16 (little endian)
   function Read_Uint16
     (BinF : in out Ada.Streams.Stream_IO.File_Type) return Unsigned_16 is
      Buffer : Ada.Streams.Stream_Element_Array (1 .. 2);
      Last : Ada.Streams.Stream_Element_Offset;
   begin
      Ada.Streams.Stream_IO.Read (BinF, Buffer, Last);
      return Unsigned_16 (Buffer (1)) +
             Unsigned_16 (Buffer (2)) * 2**8;
   end Read_Uint16;

   --  Helper function to skip bytes
   procedure Skip_Bytes
     (BinF : in out Ada.Streams.Stream_IO.File_Type; Count : Positive) is
      Current_Index : Ada.Streams.Stream_IO.Positive_Count;
   begin
      Current_Index := Ada.Streams.Stream_IO.Index (BinF);
      Ada.Streams.Stream_IO.Set_Index
        (BinF, Current_Index + Ada.Streams.Stream_IO.Positive_Count (Count));
   end Skip_Bytes;
--
--
--
   function Load_WAV_File (File_Name : String) return WAV_Data_Record is
      BinF         : Ada.Streams.Stream_IO.File_Type;
      Header       : WAV_Header;
      Data_Size    : Natural := 0;        -- Data size in bytes
      Sample_Count : Sample_Size_t;        -- Number of 16-bit samples
      Data         : Sample_16_t_Array_Access;
      Result       : WAV_Data_Record;
      Chunk_ID     : String (1 .. 4);
      Chunk_Size   : Unsigned_32;
      Fmt_Found    : Boolean := False;
      Data_Found   : Boolean := False;

   begin
      --  Open file as binary stream
      Ada.Streams.Stream_IO.Open
        (BinF, Ada.Streams.Stream_IO.In_File, File_Name);

      --  Read RIFF header
      Header.ChunkID := Read_Chunk_ID (BinF);   --  Read 4 char "RIFF"
      Header.ChunkSize := Read_Uint32 (BinF);   --  Read 4 Bytes Unsigned_32
      Header.Format := Read_Chunk_ID (BinF);    --  "WAVE"

      --  Verify this is a WAV file
      if Header.ChunkID /= "RIFF" or else Header.Format /= "WAVE" then
         Put_Line ("Error: Not a valid WAV file");
         Ada.Streams.Stream_IO.Close (BinF);
         Result.Header := Header;
         Result.Data := null;
         Result.Sample_Count := 0;
         return Result;
      end if;

      --  Parse chunks until we find fmt and data
      while
         not (Fmt_Found and Data_Found) and then
         not Ada.Streams.Stream_IO.End_Of_File (BinF) loop
         --
         Chunk_ID := Read_Chunk_ID (BinF);
         Chunk_Size := Read_Uint32 (BinF);

         if Chunk_ID = "fmt " then
            --  Read format chunk
            Header.Subchunk1ID := Chunk_ID;
            Header.Subchunk1Size := Chunk_Size;
            Header.AudioFormat := Read_Uint16 (BinF);
            Header.NumChannels := Read_Uint16 (BinF);
            Header.SampleRate := Read_Uint32 (BinF);
            Header.ByteRate := Read_Uint32 (BinF);
            Header.BlockAlign := Read_Uint16 (BinF);
            Header.BitsPerSample := Read_Uint16 (BinF);

            --  Verify this is 16-bit audio
            if Header.BitsPerSample /= 16 then
               Put_Line ("Error: Only 16-bit audio is supported, found " &
                        Unsigned_16'Image (Header.BitsPerSample) & "-bit");
               Ada.Streams.Stream_IO.Close (BinF);
               Result.Header := Header;
               Result.Data := null;
               Result.Sample_Count := 0;
               return Result;
            end if;

            --  Skip any remaining bytes in the fmt chunk
            if Chunk_Size > 16 then
               Skip_Bytes (BinF, Natural (Chunk_Size - 16));
            end if;

            Fmt_Found := True;

         elsif Chunk_ID = "data" then
            --  Found data chunk
            Header.Subchunk2ID := Chunk_ID;
            Header.Subchunk2Size := Chunk_Size;
            Data_Size := Natural (Chunk_Size);

            --  Calculate number of 16-bit samples
            Sample_Count := Sample_Size_t (Data_Size / 2);

            --  allocated memory dynamically
            Data := new Sample_Array_16_t (1 .. Sample_Size_t (Data_Size / 2));

--  Read audio data
--  Treat Data array directly as Stream_Element_Array (overlay technique)
--  This overlays the same memory area with different types.
--  Safe because Byte and Stream_Element are the same size (8 bits).
            declare
               Data_Stream_View : Stream_Element_Array
                 (1 .. Stream_Element_Offset (Data_Size));
               for Data_Stream_View'Address use Data.all'Address;
               pragma Import (Ada, Data_Stream_View);
               Last : Stream_Element_Offset;
            begin
--  Note: WAV files use little-endian format
--  If your system is big-endian, you would need to swap bytes here
--  For most modern systems (x86, x64), no conversion is needed
               Stream_IO.Read (BinF, Data_Stream_View, Last);
            end;

            Data_Found := True;

         else
            --  Skip unknown chunks (like JUNK, LIST, etc.)
            Put_Line ("Skipping chunk: " & Chunk_ID & " (size:" &
                        Unsigned_32'Image (Chunk_Size) & ")");
            Skip_Bytes (BinF, Natural (Chunk_Size));

            --  Handle odd-sized chunks (WAV chunks must be word-aligned)
            if Chunk_Size mod 2 = 1 then
               Skip_Bytes (BinF, 1);
            end if;
         end if;
      end loop;

      --  Close file
      Ada.Streams.Stream_IO.Close (BinF);

      --  Check if we found both required chunks
      if not Fmt_Found then
         Put_Line ("Error: fmt chunk not found");
         Free (Data);
         Data := null;
      end if;

      if not Data_Found then
         Put_Line ("Error: data chunk not found");
         if Data /= null then
            Free (Data);
         end if;
--  ****         Sample_Count := 0;
         Data_Size := 0;
      end if;

      --  Print header information
      Put_Line ("ChunkID=        " & Header.ChunkID);
      Put_Line ("ChunkSize=      " & Unsigned_32'Image (Header.ChunkSize));
      Put_Line ("Format=         " & Header.Format);
      Put_Line ("Subchunk1ID=    " & Header.Subchunk1ID);
      Put_Line ("Subchunk1Size=  " & Unsigned_32'Image (Header.Subchunk1Size));
      Put_Line ("Audio Format=   " & Unsigned_16'Image (Header.AudioFormat));
      Put_Line ("NumChannels=    " & Unsigned_16'Image (Header.NumChannels));
      Put_Line ("SampleRate=     " & Unsigned_32'Image (Header.SampleRate));
      Put_Line ("ByteRate=       " & Unsigned_32'Image (Header.ByteRate));
      Put_Line ("BlockAlign=     " & Unsigned_16'Image (Header.BlockAlign));
      Put_Line ("BitsPerSample=  " & Unsigned_16'Image (Header.BitsPerSample));
      Put_Line ("Subchunk2ID=    " & Header.Subchunk2ID);
      Put_Line ("Subchunk2Size=  " & Unsigned_32'Image (Header.Subchunk2Size));
      Put_Line ("Sample Count=   " & Sample_Size_t'Image (Sample_Count));

      --  Return result
      Result.Header := Header;
      Result.Data := Data;
      Result.Sample_Count := Integer (Sample_Count);
      return Result;
   end Load_WAV_File;
--
--
------------------------------------------------------
--  GLOBAL VARIABLES
------------------------------------------------------
--
   Device : OpenAL.Context.Device_t;
   Context : Context_t;
   Buffer : Buffer_t;
   Source : Source_t;
   WAV_File : WAV_Data_Record;
   Set_Active_Context : Boolean;
   Buffers : Buffer_Array_t (1 .. 1);
   Sources : Source_Array_t (1 .. 1);
   EndFlag : Integer := 0;
   ProcessedNr : Natural := 0;
--
---------------------------------------------------------
---------------------------------------------------------
--  MAIN
---------------------------------------------------------
---------------------------------------------------------
begin
--
   Put_Line ("OpenAL WAV Player starting...");
--  Open_Device works only with "OpenAL_Soft". Choose OS Setting individually
   Device := OpenAL.Context.Open_Device ("OpenAL Soft");
   if Device = Invalid_Device then
      Put_Line ("Error: Failed to open OpenAL device");
      return;
   end if;
--  Context works for 3D, Listner and Environment settings
   Context := Create_Context (Device);
   if Context = Null_Context then
      Put_Line ("Error: Failed to create OpenAL context");
      Close_Device (Device);
      return;
   end if;
--  Choose the working Context
   Set_Active_Context := Make_Context_Current (Context);
   if Set_Active_Context = False then
      Put_Line ("Error: Failed, cannot Set Active Context");
      Destroy_Context (Context);
      Close_Device (Device);
      return;
   end if;

   Generate_Buffers (Buffers);
   Generate_Sources (Sources);
   Buffer := Buffers (1);
   Source := Sources (1);

   WAV_File := Load_WAV_File (FILE_NAME);

   if WAV_File.Data = null then
      Put_Line ("Error: Failed to load WAV file");
      Destroy_Context (Context);
      Close_Device (Device);
      return;
   end if;
   Put_Line ("WAV file loaded successfully");

   --  Decide OpenAL format following WAV file format
   declare
      OpenAL_Format : OpenAL.Types.Enumeration_t;
   begin
      case WAV_File.Header.NumChannels is
         when 1 =>   --  MONO
            case WAV_File.Header.BitsPerSample is
               when 8 =>
                  OpenAL_Format := OpenAL.Thin.AL_FORMAT_MONO8;
                  Put_Line ("Using format: AL_FORMAT_MONO8");
               when 16 =>
                  OpenAL_Format := OpenAL.Thin.AL_FORMAT_MONO16;
                  Put_Line ("Using format: AL_FORMAT_MONO16");
               when others =>
                  Put_Line ("Error: Unsupported bits per sample for mono: " &
                        Unsigned_16'Image (WAV_File.Header.BitsPerSample));
                  Destroy_Context (Context);
                  Close_Device (Device);
                  if WAV_File.Data /= null then
                     Free (WAV_File.Data);
                  end if;
                  return;
            end case;

         when 2 =>   --  Stereo
            case WAV_File.Header.BitsPerSample is
               when 8 =>
                  OpenAL_Format := OpenAL.Thin.AL_FORMAT_STEREO8;
                  Put_Line ("Using format: AL_FORMAT_STEREO8");
               when 16 =>
                  OpenAL_Format := OpenAL.Thin.AL_FORMAT_STEREO16;
                  Put_Line ("Using format: AL_FORMAT_STEREO16");
               when others =>
                  Put_Line ("Error: Unsupported bits per sample for stereo: " &
                            Unsigned_16'Image (WAV_File.Header.BitsPerSample));
                  Destroy_Context (Context);
                  Close_Device (Device);
                  if WAV_File.Data /= null then
                     Free (WAV_File.Data);
                  end if;
                  return;
            end case;

         when others =>
            Put_Line ("Error: Unsupported number of channels: " &
                        Unsigned_16'Image (WAV_File.Header.NumChannels));
            Destroy_Context (Context);
            Close_Device (Device);
            if WAV_File.Data /= null then
               Free (WAV_File.Data);
            end if;
            return;
      end case;

      --  Set buffer data with determined format
      OpenAL.Thin.Buffer_Data (
         Buffer_ID => 1,
         Format    => OpenAL_Format,
         Data      => WAV_File.Data (1)'Address,
         Size => WAV_File.Data.all'Length,
         Frequency => OpenAL.Types.Size_t (WAV_File.Header.SampleRate)
      );
   end;

   --  Configure source
   OpenAL.Source.Set_Current_Buffer (Source, Buffer);
   OpenAL.Source.Set_Pitch (Source, 1.0);
   OpenAL.Source.Set_Gain (Source, 1.0);
   OpenAL.Source.Set_Position_Float (Source, 0.0, 0.0, 0.0);
   OpenAL.Source.Set_Velocity_Float (Source, 0.0, 0.0, 0.0);
   OpenAL.Source.Set_Looping (Source, False);

   Put_Line ("Size => WAV_File.Data.all'Length," &
               Integer'Image (WAV_File.Data.all'Length));

   Put_Line ("Starting audio playback...");

   --  Start playback
   OpenAL.Source.Play (Source);

   --  Wait until playback finishes
   loop
      OpenAL.Thin.Get_Sourcei
         (Source_ID => OpenAL.Types.Unsigned_Integer_t (1),
         Parameter => OpenAL.Thin.AL_SOURCE_STATE,
         Value => EndFlag'Address);
      exit when EndFlag /= OpenAL.Thin.AL_PLAYING;
      delay 0.1;
   end loop;

   Put_Line ("Playback completed");

   --  Clean up resources
   if WAV_File.Data /= null then
      Free (WAV_File.Data);
   end if;
   OpenAL.Source.Get_Buffers_Queued (Source => Source,
                                  Buffers => ProcessedNr);  --  Natural
   if ProcessedNr /= 0 then
      OpenAL.Source.Unqueue_Buffers (Source => Source,
                                  Buffers => Buffers); --  Buffer_Array_t
   end if;
   OpenAL.Buffer.Delete_Buffers (Buffers);
   OpenAL.Context.Close_Device (Device);    --  "OpenAL Soft")
   OpenAL.Context.Destroy_Context (Context);
   OpenAL.Source.Delete_Sources (Sources);
--
--
end Play_Wav;
