unit FLAC;

interface

uses
  {$IFDEF WIN32}
   Windows;
  {$ENDIF}

  {$IFDEF LINUX}
  Libc, ACS_Procs;
  {$ENDIF}

var

  LibFLACLoaded : Boolean = False;

const

 {$IFDEF WIN32}
  LibFLACPath = 'libFLAC.dll';
 {$ENDIF}

 {$IFDEF LINUX}
  LibFLACPath = 'libFLAC.so*'; // libFLAC.so
  {$DEFINE SEARCH_LIBS}
 {$ENDIF}

  FLAC__STREAM_ENCODER_OK = 0;
  FLAC__STREAM_ENCODER_STREAM_ENCODER_ERROR = 1;
  FLAC__STREAM_ENCODER_MEMORY_ALLOCATION_ERROR = 2;
  FLAC__STREAM_ENCODER_WRITE_ERROR = 3;
  FLAC__STREAM_ENCODER_READ_ERROR = 4;
  FLAC__STREAM_ENCODER_SEEK_ERROR = 5;
  FLAC__STREAM_ENCODER_ALREADY_INITIALIZED = 6;
  FLAC__STREAM_ENCODER_INVALID_CALLBACK = 7;
  FLAC__STREAM_ENCODER_INVALID_SEEKTABLE = 8;
  FLAC__STREAM_ENCODER_UNINITIALIZED = 9;

  FLAC__STREAM_ENCODER_SEEK_STATUS_OK = 0;
  FLAC__STREAM_ENCODER_SEEK_STATUS_ERROR = 1;

  FLAC__STREAM_ENCODER_TELL_STATUS_OK = 0;
  FLAC__STREAM_ENCODER_TELL_STATUS_ERROR = 1;

  FLAC__STREAM_DECODER_SEARCH_FOR_METADATA = 0;
	//**< The decoder is ready to search for metadata. */

  FLAC__STREAM_DECODER_READ_METADATA = 1;
        //**< The decoder is ready to or is in the process of reading metadata. */

  FLAC__STREAM_DECODER_SEARCH_FOR_FRAME_SYNC  = 2;
	//**< The decoder is ready to or is in the process of searching for the
	// * frame sync code.
	// */

  FLAC__STREAM_DECODER_READ_FRAME = 3;
        ///**< The decoder is ready to or is in the process of reading a frame. */

  FLAC__STREAM_DECODER_END_OF_STREAM = 4;
	///**< The decoder has reached the end of the stream. */

  FLAC__STREAM_DECODER_OGG_ERROR = 5;
	///**< An error occurred in the underlying Ogg layer.  */

  FLAC__STREAM_DECODER_SEEK_ERROR = 6;
	///**< An error occurred while seeking.  The decoder must be flushed
	// * with FLAC__stream_decoder_flush() or reset with
	// * FLAC__stream_decoder_reset() before decoding can continue.
	// */

  FLAC__STREAM_DECODER_ABORTED = 7;
	// /**< The decoder was aborted by the read callback. */

  FLAC__STREAM_DECODER_MEMORY_ALLOCATION_ERROR = 8;
	// /**< An error occurred allocating memory.  The decoder is in an invalid
	// * state and can no longer be used.
	// */

  FLAC__STREAM_DECODER_UNINITIALIZED = 9;
	///**< The decoder is in the uninitialized state; one of the
	// * FLAC__stream_decoder_init_*() functions must be called before samples
	// * can be processed.
	// */


  FLAC__STREAM_DECODER_READ_STATUS_OK = 0;
  FLAC__STREAM_DECODER_READ_STATUS_ERROR = 1;

  FLAC__STREAM_DECODER_SEEK_STATUS_OK = 0;
  FLAC__STREAM_DECODER_SEEK_STATUS_ERROR = 1;

  FLAC__STREAM_DECODER_TELL_STATUS_OK = 0;
  FLAC__STREAM_DECODER_TELL_STATUS_ERROR = 1;

  FLAC__STREAM_DECODER_LENGTH_STATUS_OK = 0;
  FLAC__STREAM_DECODER_LENGTH_STATUS_ERROR = 1;

type

  FLAC__byte = Byte;
  PFLAC__byte = ^FLAC__byte;

  FLAC__uint64 = Int64;
  PFLAC__uint64 = ^FLAC__uint64;

  FLAC__uint32 = LongWord;
  PFLAC__uint32 = ^FLAC__uint32;

  FLAC__int32 = Integer;
  PFLAC__int32 = ^FLAC__int32;

  FLAC__bool = LongBool;

  PFLAC__StreamEncoder = Pointer;

  PFLAC__StreamDecoder = Pointer;

  FLAC__MetadataType = Integer;

  PFLAC__Frame = Pointer;

  FLAC__FrameHeader = packed record
    blocksize : LongWord;
    sample_rate : LongWord;
    channels : LongWord;
    channel_assignment : Integer;
    bits_per_sample : LongWord;
    frame_number : FLAC__uint32;
    crc : Byte;
  end;

  FLACInfo = packed record
    min_blocksize : LongWord;
    max_blocksize : LongWord;
    min_framesize : LongWord;
    max_framesize : LongWord;
    sample_rate : LongWord;
    channels : LongWord;
    bits_per_sample : LongWord;
    total_samples1 : LongWord;
    total_samples2 : LongWord;
  end;
  PFLACInfo = ^FLACInfo;

  unsigned = LongWord;

  FLAC_StreamInfo = packed record
    min_blocksize, max_blocksize,
    min_framesize, max_framesize,
    sample_rate,
    channels,
    bits_per_sample: unsigned;
    // ??? Suddenly we need this dummy, some misalignment ???
    dummy: unsigned;
    // ???
    total_samples: FLAC__uint64;
    md5sum: array[0..15] of FLAC__byte;
  end;
  {
typedef struct
	unsigned min_blocksize, max_blocksize;
	unsigned min_framesize, max_framesize;
	unsigned sample_rate;
	unsigned channels;
	unsigned bits_per_sample;
	FLAC__uint64 total_samples;
	FLAC__byte md5sum[16];
 FLAC__StreamMetadata_StreamInfo;
  }

{00690 typedef struct {
00691     FLAC__MetadataType type;
00696     FLAC__bool is_last;
00699     unsigned length;
00702     union {
00703         FLAC__StreamMetadata_StreamInfo stream_info;
00704         FLAC__StreamMetadata_Padding padding;
00705         FLAC__StreamMetadata_Application application;
00706         FLAC__StreamMetadata_SeekTable seek_table;
00707         FLAC__StreamMetadata_VorbisComment vorbis_comment;
00708         FLAC__StreamMetadata_CueSheet cue_sheet;
00709         FLAC__StreamMetadata_Unknown unknown;
}
  FLAC__StreamMetadata = packed record
    metadataType: FLAC__MetadataType;
    is_last:      FLAC__bool;
    length:       unsigned;
    dummy:        unsigned;
    data: FLAC_StreamInfo;
  end;
  PFLAC__StreamMetadata = ^FLAC__StreamMetadata;

  PFLAC__FrameHeader = ^FLAC__FrameHeader;

  FLACIntBuf = array[0..0] of FLAC__int32;
  PFLACIntBuf = ^FLACIntBuf;

  FLACChannels = array[0..1] of PFLACIntBuf;
  PFLACChannels = ^FLACChannels;

  FLAC__StreamEncoderWriteCallback  = function(encoder : PFLAC__StreamEncoder;
                                              buffer : PFLAC__byte;
                                              bytes,
                                              samples,
                                              current_frame : LongWord;
                                              client_data : Pointer) : Integer; cdecl;

  FLAC__StreamEncoderSeekCallback  = function(encoder : PFLAC__StreamEncoder;
                                             absolute_byte_offset : FLAC__uint64;
                                             client_data : Pointer) : Integer; cdecl;

  FLAC__StreamEncoderTellCallback = function(encoder: PFLAC__StreamEncoder;
                                             var absolute_byte_offset: FLAC__uint64;
                                             client_data: Pointer): Integer; cdecl;

  FLAC__StreamEncoderMetadataCallback = procedure(encoder: PFLAC__StreamEncoder;
                                              const metadata: PFLAC__StreamMetadata;
                                              client_data : Pointer); cdecl;

  FLAC__StreamDecoderReadCallback = function(decoder : PFLAC__StreamDecoder;
                                                     buffer : PFLAC__byte;
                                                     var bytes : LongWord;
                                                     client_data : Pointer) : Integer; cdecl;

  FLAC__StreamDecoderSeekCallback = function(decoder : PFLAC__StreamDecoder;
                                                     absolute_byte_offset : FLAC__uint64;
                                                     client_data : Pointer) : Integer; cdecl;

  FLAC__StreamDecoderTellCallback = function(decoder : PFLAC__StreamDecoder;
                                                     var absolute_byte_offset : FLAC__uint64;
                                                     client_data : Pointer) : Integer; cdecl;

  FLAC__StreamDecoderLengthCallback = function(decoder : PFLAC__StreamDecoder;
                                                       var stream_length : FLAC__uint64;
                                                       client_data : Pointer) : Integer; cdecl;

  FLAC__StreamDecoderEofCallback = function(decoder : PFLAC__StreamDecoder;
                                                    client_data : Pointer) : FLAC__bool; cdecl;

  FLAC__StreamDecoderWriteCallback = function(decoder : PFLAC__StreamDecoder;
                                                      frame : PFLAC__FrameHeader;
                                                      buffer : PFLACChannels;
                                                      client_data : Pointer) : Integer; cdecl;

  FLAC__StreamDecoderMetadataCallback = procedure(decoder : PFLAC__StreamDecoder;
                                                          metadata : PFLAC__StreamMetadata;
                                                          client_data : Pointer); cdecl;

  FLAC__StreamDecoderErrorCallback = procedure(decoder : PFLAC__StreamDecoder;
                                                      status : Integer;
                                                      client_data : Pointer); cdecl;

  FLAC__stream_encoder_new_t = function : PFLAC__StreamEncoder; cdecl;
  FLAC__stream_encoder_delete_t = procedure(encoder : PFLAC__StreamEncoder); cdecl;
  FLAC__stream_encoder_init_stream_t = function(encoder: PFLAC__StreamEncoder;
		write_callback: FLAC__StreamEncoderWriteCallback;
		seek_callback:  FLAC__StreamEncoderSeekCallback;
		tell_callback:  FLAC__StreamEncoderTellCallback;
		metadata_callback: FLAC__StreamEncoderMetadataCallback;
		client_data: pointer): Integer; cdecl;
  FLAC__stream_encoder_set_verify_t = function( encoder : PFLAC__StreamEncoder; value : Boolean) : Boolean; cdecl;
  FLAC__stream_encoder_set_streamable_subset_t = function( encoder : PFLAC__StreamEncoder; value : Boolean) : Boolean; cdecl;
  FLAC__stream_encoder_set_do_mid_side_stereo_t = function( encoder : PFLAC__StreamEncoder; value : Boolean) : Boolean; cdecl;
  FLAC__stream_encoder_set_loose_mid_side_stereo_t = function( encoder : PFLAC__StreamEncoder; value : Boolean) : Boolean; cdecl;
  FLAC__stream_encoder_set_channels_t = function( encoder : PFLAC__StreamEncoder; value : LongWord) : Boolean; cdecl;
  FLAC__stream_encoder_set_bits_per_sample_t = function( encoder : PFLAC__StreamEncoder; value : LongWord) : Boolean; cdecl;
  FLAC__stream_encoder_set_sample_rate_t = function( encoder : PFLAC__StreamEncoder; value : LongWord) : Boolean; cdecl;
  FLAC__stream_encoder_set_blocksize_t = function( encoder : PFLAC__StreamEncoder; value : LongWord) : Boolean; cdecl;
  FLAC__stream_encoder_set_max_lpc_order_t = function( encoder : PFLAC__StreamEncoder; value : LongWord) : Boolean; cdecl;
  FLAC__stream_encoder_set_qlp_coeff_precision_t = function( encoder : PFLAC__StreamEncoder; value : LongWord) : Boolean; cdecl;
  FLAC__stream_encoder_set_do_qlp_coeff_prec_search_t = function( encoder : PFLAC__StreamEncoder; value : Boolean) : Boolean; cdecl;
  FLAC__stream_encoder_set_do_escape_coding_t = function( encoder : PFLAC__StreamEncoder; value : Boolean) : Boolean; cdecl;
  FLAC__stream_encoder_set_do_exhaustive_model_search_t = function( encoder : PFLAC__StreamEncoder; value : Boolean) : Boolean; cdecl;
  FLAC__stream_encoder_set_min_residual_partition_order_t = function( encoder : PFLAC__StreamEncoder; value : LongWord) : Boolean; cdecl;
  FLAC__stream_encoder_set_max_residual_partition_order_t = function( encoder : PFLAC__StreamEncoder; value : LongWord) : Boolean; cdecl;
  FLAC__stream_encoder_set_rice_parameter_search_dist_t = function( encoder : PFLAC__StreamEncoder; value : LongWord) : Boolean; cdecl;
  FLAC__stream_encoder_set_total_samples_estimate_t = function( encoder : PFLAC__StreamEncoder; value : FLAC__uint64) : Boolean; cdecl;
  FLAC__stream_encoder_set_metadata_t = function( encoder : PFLAC__StreamEncoder; metadata : Pointer; num_blocks : LongWord) : Boolean; cdecl;
  FLAC__stream_encoder_get_state_t = function( encoder : PFLAC__StreamEncoder) : Integer; cdecl;
  FLAC__stream_encoder_get_stream_encoder_state_t = function( encoder : PFLAC__StreamEncoder) : Integer; cdecl;
  FLAC__stream_encoder_get_verify_decoder_state_t = function( encoder : PFLAC__StreamEncoder) : Integer; cdecl;
  FLAC__stream_encoder_get_resolved_state_string_t = function( encoder : PFLAC__StreamEncoder) : PChar; cdecl;
  FLAC__stream_encoder_get_verify_decoder_error_stats_t = procedure(encoder : PFLAC__StreamEncoder; absolute_sample : PFLAC__uint64; frame_number, channel, sample : PLongWord; expected, got : FLAC__int32);  cdecl;
  FLAC__stream_encoder_get_verify_t = function( encoder : PFLAC__StreamEncoder) : Boolean; cdecl;
  FLAC__stream_encoder_get_streamable_subset_t = function( encoder : PFLAC__StreamEncoder) : Boolean; cdecl;
  FLAC__stream_encoder_get_do_mid_side_stereo_t = function( encoder : PFLAC__StreamEncoder) : Boolean; cdecl;
  FLAC__stream_encoder_get_loose_mid_side_stereo_t = function( encoder : PFLAC__StreamEncoder) : Boolean; cdecl;
  FLAC__stream_encoder_get_channels_t = function( encoder : PFLAC__StreamEncoder) : LongWord; cdecl;
  FLAC__stream_encoder_get_bits_per_sample_t = function( encoder : PFLAC__StreamEncoder) : LongWord; cdecl;
  FLAC__stream_encoder_get_sample_rate_t = function( encoder : PFLAC__StreamEncoder) : LongWord; cdecl;
  FLAC__stream_encoder_get_blocksize_t = function( encoder : PFLAC__StreamEncoder) : LongWord; cdecl;
  FLAC__stream_encoder_get_max_lpc_order_t = function( encoder : PFLAC__StreamEncoder) : LongWord; cdecl;
  FLAC__stream_encoder_get_qlp_coeff_precision_t = function( encoder : PFLAC__StreamEncoder) : LongWord; cdecl;
  FLAC__stream_encoder_get_do_qlp_coeff_prec_search_t = function( encoder : PFLAC__StreamEncoder) : Boolean; cdecl;
  FLAC__stream_encoder_get_do_escape_coding_t = function( encoder : PFLAC__StreamEncoder) : Boolean; cdecl;
  FLAC__stream_encoder_get_do_exhaustive_model_search_t = function( encoder : PFLAC__StreamEncoder) : Boolean; cdecl;
  FLAC__stream_encoder_get_min_residual_partition_order_t = function( encoder : PFLAC__StreamEncoder) : LongWord; cdecl;
  FLAC__stream_encoder_get_max_residual_partition_order_t = function( encoder : PFLAC__StreamEncoder) : LongWord; cdecl;
  FLAC__stream_encoder_get_rice_parameter_search_dist_t = function( encoder : PFLAC__StreamEncoder) : LongWord; cdecl;
  FLAC__stream_encoder_get_total_samples_estimate_t = function( encoder : PFLAC__StreamEncoder) : FLAC__uint64 cdecl;
  FLAC__stream_encoder_init_t = function( encoder : PFLAC__StreamEncoder) : Integer; cdecl;
  FLAC__stream_encoder_finish_t = procedure(encoder : PFLAC__StreamEncoder); cdecl;
  FLAC__stream_encoder_process_t = function(encoder : PFLAC__StreamEncoder; buffer : PFLAC__int32; samples : LongWord) : Boolean; cdecl;
  FLAC__stream_encoder_process_interleaved_t = function(encoder : PFLAC__StreamEncoder; buffer : PFLAC__int32; samples : LongWord) : Boolean; cdecl;

  FLAC__stream_decoder_new_t = function : PFLAC__StreamDecoder; cdecl;
  FLAC__stream_decoder_delete_t = procedure(decoder : PFLAC__StreamDecoder); cdecl;

  FLAC__stream_decoder_init_stream_t = function(decoder: PFLAC__StreamDecoder;
		read_callback: FLAC__StreamDecoderReadCallback  	;
		seek_callback: FLAC__StreamDecoderSeekCallback  	;
		tell_callback: FLAC__StreamDecoderTellCallback  	;
		length_callback: FLAC__StreamDecoderLengthCallback  	;
		eof_callback: FLAC__StreamDecoderEofCallback  	;
		write_callback: FLAC__StreamDecoderWriteCallback  	;
		metadata_callback: FLAC__StreamDecoderMetadataCallback  	;
		error_callback: FLAC__StreamDecoderErrorCallback;
		client_data: pointer) : Integer; cdecl;

  FLAC__stream_decoder_set_md5_checking_t = function( decoder : PFLAC__StreamDecoder; value : Boolean) : Boolean; cdecl;
  FLAC__stream_decoder_set_metadata_respond_t = function( decoder : PFLAC__StreamDecoder; _type : FLAC__MetadataType) : Boolean; cdecl;
  FLAC__stream_decoder_set_metadata_respond_application_t = function( decoder : PFLAC__StreamDecoder; id : PFLAC__byte) : Boolean; cdecl;
  FLAC__stream_decoder_set_metadata_respond_all_t = function( decoder : PFLAC__StreamDecoder) : Boolean; cdecl;
  FLAC__stream_decoder_set_metadata_ignore_t = function( decoder : PFLAC__StreamDecoder; _type : FLAC__MetadataType) : Boolean; cdecl;
  FLAC__stream_decoder_set_metadata_ignore_application_t = function( decoder : PFLAC__StreamDecoder; id : PFLAC__byte) : Boolean; cdecl;
  FLAC__stream_decoder_set_metadata_ignore_all_t = function( decoder : PFLAC__StreamDecoder) : Boolean; cdecl;
  FLAC__stream_decoder_get_state_t = function( decoder : PFLAC__StreamDecoder) : Integer; cdecl;
  FLAC__stream_decoder_get_stream_decoder_state_t = function( decoder : PFLAC__StreamDecoder) : Integer; cdecl;
  FLAC__stream_decoder_get_resolved_state_string_t = function( decoder : PFLAC__StreamDecoder) : PChar; cdecl;
  FLAC__stream_decoder_get_md5_checking_t = function( decoder : PFLAC__StreamDecoder) : Boolean; cdecl;
  FLAC__stream_decoder_get_channels_t = function( decoder : PFLAC__StreamDecoder) : Integer; cdecl;
  FLAC__stream_decoder_get_channel_assignment_t = function( decoder : PFLAC__StreamDecoder) : Integer; cdecl;
  FLAC__stream_decoder_get_bits_per_sample_t = function( decoder : PFLAC__StreamDecoder) : LongWord; cdecl;
  FLAC__stream_decoder_get_sample_rate_t = function( decoder : PFLAC__StreamDecoder) : LongWord; cdecl;
  FLAC__stream_decoder_get_blocksize_t = function( decoder : PFLAC__StreamDecoder) : LongWord; cdecl;
  FLAC__stream_decoder_get_decode_position_t = function( decoder : PFLAC__StreamDecoder; var position : FLAC__uint64) : Boolean; cdecl;
  FLAC__stream_decoder_finish_t = function( decoder : PFLAC__StreamDecoder) : Boolean; cdecl;
  FLAC__stream_decoder_flush_t = function( decoder : PFLAC__StreamDecoder) : Boolean; cdecl;
  FLAC__stream_decoder_reset_t = function( decoder : PFLAC__StreamDecoder) : Boolean; cdecl;
  FLAC__stream_decoder_process_single_t = function( decoder : PFLAC__StreamDecoder) : Boolean; cdecl;
  FLAC__stream_decoder_process_until_end_of_metadata_t = function( decoder : PFLAC__StreamDecoder) : Boolean; cdecl;
  FLAC__stream_decoder_process_until_end_of_stream_t = function( decoder : PFLAC__StreamDecoder) : Boolean; cdecl;
  FLAC__stream_decoder_seek_absolute_t = function( decoder : PFLAC__StreamDecoder; sample : FLAC__uint64) : Boolean; cdecl;

var

  FLAC__stream_encoder_new : FLAC__stream_encoder_new_t;
  FLAC__stream_encoder_delete : FLAC__stream_encoder_delete_t;
  FLAC__stream_encoder_init_stream: FLAC__stream_encoder_init_stream_t;
  FLAC__stream_encoder_set_verify : FLAC__stream_encoder_set_verify_t;
  FLAC__stream_encoder_set_streamable_subset : FLAC__stream_encoder_set_streamable_subset_t;
  FLAC__stream_encoder_set_do_mid_side_stereo : FLAC__stream_encoder_set_do_mid_side_stereo_t;
  FLAC__stream_encoder_set_loose_mid_side_stereo : FLAC__stream_encoder_set_loose_mid_side_stereo_t;
  FLAC__stream_encoder_set_channels : FLAC__stream_encoder_set_channels_t;
  FLAC__stream_encoder_set_bits_per_sample : FLAC__stream_encoder_set_bits_per_sample_t;
  FLAC__stream_encoder_set_sample_rate : FLAC__stream_encoder_set_sample_rate_t;
  FLAC__stream_encoder_set_blocksize : FLAC__stream_encoder_set_blocksize_t;
  FLAC__stream_encoder_set_max_lpc_order : FLAC__stream_encoder_set_max_lpc_order_t;
  FLAC__stream_encoder_set_qlp_coeff_precision : FLAC__stream_encoder_set_qlp_coeff_precision_t;
  FLAC__stream_encoder_set_do_qlp_coeff_prec_search : FLAC__stream_encoder_set_do_qlp_coeff_prec_search_t;
  FLAC__stream_encoder_set_do_escape_coding : FLAC__stream_encoder_set_do_escape_coding_t;
  FLAC__stream_encoder_set_do_exhaustive_model_search : FLAC__stream_encoder_set_do_exhaustive_model_search_t;
  FLAC__stream_encoder_set_min_residual_partition_order : FLAC__stream_encoder_set_min_residual_partition_order_t;
  FLAC__stream_encoder_set_max_residual_partition_order : FLAC__stream_encoder_set_max_residual_partition_order_t;
//  FLAC__stream_encoder_set_rice_parameter_search_dist : FLAC__stream_encoder_set_rice_parameter_search_dist_t;
  FLAC__stream_encoder_set_total_samples_estimate : FLAC__stream_encoder_set_total_samples_estimate_t;
  FLAC__stream_encoder_set_metadata : FLAC__stream_encoder_set_metadata_t;
  FLAC__stream_encoder_get_state : FLAC__stream_encoder_get_state_t;
  FLAC__stream_encoder_get_stream_encoder_state : FLAC__stream_encoder_get_stream_encoder_state_t;
  FLAC__stream_encoder_get_verify_decoder_state : FLAC__stream_encoder_get_verify_decoder_state_t;
  FLAC__stream_encoder_get_resolved_state_string : FLAC__stream_encoder_get_resolved_state_string_t;
  FLAC__stream_encoder_get_verify_decoder_error_stats : FLAC__stream_encoder_get_verify_decoder_error_stats_t;
  FLAC__stream_encoder_get_verify : FLAC__stream_encoder_get_verify_t;
  FLAC__stream_encoder_get_streamable_subset : FLAC__stream_encoder_get_streamable_subset_t;
  FLAC__stream_encoder_get_do_mid_side_stereo : FLAC__stream_encoder_get_do_mid_side_stereo_t;
  FLAC__stream_encoder_get_loose_mid_side_stereo : FLAC__stream_encoder_get_loose_mid_side_stereo_t;
  FLAC__stream_encoder_get_channels : FLAC__stream_encoder_get_channels_t;
  FLAC__stream_encoder_get_bits_per_sample : FLAC__stream_encoder_get_bits_per_sample_t;
  FLAC__stream_encoder_get_sample_rate : FLAC__stream_encoder_get_sample_rate_t;
  FLAC__stream_encoder_get_blocksize : FLAC__stream_encoder_get_blocksize_t;
  FLAC__stream_encoder_get_max_lpc_order : FLAC__stream_encoder_get_max_lpc_order_t;
  FLAC__stream_encoder_get_qlp_coeff_precision : FLAC__stream_encoder_get_qlp_coeff_precision_t;
  FLAC__stream_encoder_get_do_qlp_coeff_prec_search : FLAC__stream_encoder_get_do_qlp_coeff_prec_search_t;
  FLAC__stream_encoder_get_do_escape_coding : FLAC__stream_encoder_get_do_escape_coding_t;
  FLAC__stream_encoder_get_do_exhaustive_model_search : FLAC__stream_encoder_get_do_exhaustive_model_search_t;
  FLAC__stream_encoder_get_min_residual_partition_order : FLAC__stream_encoder_get_min_residual_partition_order_t;
  FLAC__stream_encoder_get_max_residual_partition_order : FLAC__stream_encoder_get_max_residual_partition_order_t;
// FLAC__stream_encoder_get_rice_parameter_search_dist  : FLAC__stream_encoder_get_rice_parameter_search_dist_t;
  FLAC__stream_encoder_get_total_samples_estimate : FLAC__stream_encoder_get_total_samples_estimate_t;
  FLAC__stream_encoder_finish : FLAC__stream_encoder_finish_t;
  FLAC__stream_encoder_process : FLAC__stream_encoder_process_t;
  FLAC__stream_encoder_process_interleaved : FLAC__stream_encoder_process_interleaved_t;

  FLAC__stream_decoder_new : FLAC__stream_decoder_new_t;
  FLAC__stream_decoder_delete : FLAC__stream_decoder_delete_t;
  FLAC__stream_decoder_init_stream : FLAC__stream_decoder_init_stream_t;
  FLAC__stream_decoder_set_md5_checking : FLAC__stream_decoder_set_md5_checking_t;
  FLAC__stream_decoder_set_metadata_respond : FLAC__stream_decoder_set_metadata_respond_t;
  FLAC__stream_decoder_set_metadata_respond_application : FLAC__stream_decoder_set_metadata_respond_application_t;
  FLAC__stream_decoder_set_metadata_respond_all : FLAC__stream_decoder_set_metadata_respond_all_t;
  FLAC__stream_decoder_set_metadata_ignore : FLAC__stream_decoder_set_metadata_ignore_t;
  FLAC__stream_decoder_set_metadata_ignore_application : FLAC__stream_decoder_set_metadata_ignore_application_t;
  FLAC__stream_decoder_set_metadata_ignore_all : FLAC__stream_decoder_set_metadata_ignore_all_t;
  FLAC__stream_decoder_get_state : FLAC__stream_decoder_get_state_t;
  FLAC__stream_decoder_get_stream_decoder_state : FLAC__stream_decoder_get_stream_decoder_state_t;
  FLAC__stream_decoder_get_resolved_state_string : FLAC__stream_decoder_get_resolved_state_string_t;
  FLAC__stream_decoder_get_md5_checking : FLAC__stream_decoder_get_md5_checking_t;
  FLAC__stream_decoder_get_channels : FLAC__stream_decoder_get_channels_t;
  FLAC__stream_decoder_get_channel_assignment : FLAC__stream_decoder_get_channel_assignment_t;
  FLAC__stream_decoder_get_bits_per_sample : FLAC__stream_decoder_get_bits_per_sample_t;
  FLAC__stream_decoder_get_sample_rate : FLAC__stream_decoder_get_sample_rate_t;
  FLAC__stream_decoder_get_blocksize : FLAC__stream_decoder_get_blocksize_t;
  FLAC__stream_decoder_get_decode_position : FLAC__stream_decoder_get_decode_position_t;
  FLAC__stream_decoder_finish : FLAC__stream_decoder_finish_t;
  FLAC__stream_decoder_flush : FLAC__stream_decoder_flush_t;
  FLAC__stream_decoder_reset : FLAC__stream_decoder_reset_t;
  FLAC__stream_decoder_process_single : FLAC__stream_decoder_process_single_t;
  FLAC__stream_decoder_process_until_end_of_metadata : FLAC__stream_decoder_process_until_end_of_metadata_t;
  FLAC__stream_decoder_process_until_end_of_stream : FLAC__stream_decoder_process_until_end_of_stream_t;
  FLAC__stream_decoder_seek_absolute : FLAC__stream_decoder_seek_absolute_t;

implementation

{$IFDEF WIN32}

var
  Libhandle : HMODULE;

initialization

  Libhandle := LoadLibraryEx(LibFLACPath, 0, 0);

  if Libhandle <> 0 then
  begin
    LibFLACLoaded := True;

    FLAC__stream_encoder_new := GetProcAddress(Libhandle, 'FLAC__stream_encoder_new');
    FLAC__stream_encoder_delete := GetProcAddress(Libhandle, 'FLAC__stream_encoder_delete');
    FLAC__stream_encoder_init_stream := GetProcAddress(Libhandle, 'FLAC__stream_encoder_init_stream');
    FLAC__stream_encoder_set_verify := GetProcAddress(Libhandle, 'FLAC__stream_encoder_set_verify');
    FLAC__stream_encoder_set_streamable_subset := GetProcAddress(Libhandle, 'FLAC__stream_encoder_set_streamable_subset');
    FLAC__stream_encoder_set_do_mid_side_stereo := GetProcAddress(Libhandle, 'FLAC__stream_encoder_set_do_mid_side_stereo');
    FLAC__stream_encoder_set_loose_mid_side_stereo := GetProcAddress(Libhandle, 'FLAC__stream_encoder_set_loose_mid_side_stereo');
    FLAC__stream_encoder_set_channels := GetProcAddress(Libhandle, 'FLAC__stream_encoder_set_channels');
    FLAC__stream_encoder_set_bits_per_sample := GetProcAddress(Libhandle, 'FLAC__stream_encoder_set_bits_per_sample');
    FLAC__stream_encoder_set_sample_rate := GetProcAddress(Libhandle, 'FLAC__stream_encoder_set_sample_rate');
    FLAC__stream_encoder_set_blocksize := GetProcAddress(Libhandle, 'FLAC__stream_encoder_set_blocksize');
    FLAC__stream_encoder_set_max_lpc_order := GetProcAddress(Libhandle, 'FLAC__stream_encoder_set_max_lpc_order');
    FLAC__stream_encoder_set_qlp_coeff_precision := GetProcAddress(Libhandle, 'FLAC__stream_encoder_set_qlp_coeff_precision');
    FLAC__stream_encoder_set_do_qlp_coeff_prec_search := GetProcAddress(Libhandle, 'FLAC__stream_encoder_set_do_qlp_coeff_prec_search');
    FLAC__stream_encoder_set_do_escape_coding := GetProcAddress(Libhandle, 'FLAC__stream_encoder_set_do_escape_coding');
    FLAC__stream_encoder_set_do_exhaustive_model_search := GetProcAddress(Libhandle, 'FLAC__stream_encoder_set_do_exhaustive_model_search');
    FLAC__stream_encoder_set_min_residual_partition_order := GetProcAddress(Libhandle, 'FLAC__stream_encoder_set_min_residual_partition_order');
    FLAC__stream_encoder_set_max_residual_partition_order := GetProcAddress(Libhandle, 'FLAC__stream_encoder_set_max_residual_partition_order');
  //    FLAC__stream_encoder_set_rice_parameter_search_dist := GetProcAddress(Libhandle, 'FLAC__stream_encoder_set_rice_parameter_search_dist');
    FLAC__stream_encoder_set_total_samples_estimate := GetProcAddress(Libhandle, 'FLAC__stream_encoder_set_total_samples_estimate');
    FLAC__stream_encoder_set_metadata := GetProcAddress(Libhandle, 'FLAC__stream_encoder_set_metadata');
    FLAC__stream_encoder_get_state := GetProcAddress(Libhandle, 'FLAC__stream_encoder_get_state');
    FLAC__stream_encoder_get_stream_encoder_state := GetProcAddress(Libhandle, 'FLAC__stream_encoder_get_stream_encoder_state');
    FLAC__stream_encoder_get_verify_decoder_state := GetProcAddress(Libhandle, 'FLAC__stream_encoder_get_verify_decoder_state');
    FLAC__stream_encoder_get_resolved_state_string := GetProcAddress(Libhandle, 'FLAC__stream_encoder_get_resolved_state_string');
    FLAC__stream_encoder_get_verify_decoder_error_stats := GetProcAddress(Libhandle, 'FLAC__stream_encoder_get_verify_decoder_error_stats');
    FLAC__stream_encoder_get_verify := GetProcAddress(Libhandle, 'FLAC__stream_encoder_get_verify');
    FLAC__stream_encoder_get_streamable_subset := GetProcAddress(Libhandle, 'FLAC__stream_encoder_get_streamable_subset');
    FLAC__stream_encoder_get_do_mid_side_stereo := GetProcAddress(Libhandle, 'FLAC__stream_encoder_get_do_mid_side_stereo');
    FLAC__stream_encoder_get_loose_mid_side_stereo := GetProcAddress(Libhandle, 'FLAC__stream_encoder_get_loose_mid_side_stereo');
    FLAC__stream_encoder_get_channels := GetProcAddress(Libhandle, 'FLAC__stream_encoder_get_channels');
    FLAC__stream_encoder_get_bits_per_sample := GetProcAddress(Libhandle, 'FLAC__stream_encoder_get_bits_per_sample');
    FLAC__stream_encoder_get_sample_rate := GetProcAddress(Libhandle, 'FLAC__stream_encoder_get_sample_rate');
    FLAC__stream_encoder_get_blocksize := GetProcAddress(Libhandle, 'FLAC__stream_encoder_get_blocksize');
    FLAC__stream_encoder_get_max_lpc_order := GetProcAddress(Libhandle, 'FLAC__stream_encoder_get_max_lpc_order');
    FLAC__stream_encoder_get_qlp_coeff_precision := GetProcAddress(Libhandle, 'FLAC__stream_encoder_get_qlp_coeff_precision');
    FLAC__stream_encoder_get_do_qlp_coeff_prec_search := GetProcAddress(Libhandle, 'FLAC__stream_encoder_get_do_qlp_coeff_prec_search');
    FLAC__stream_encoder_get_do_escape_coding := GetProcAddress(Libhandle, 'FLAC__stream_encoder_get_do_escape_coding');
    FLAC__stream_encoder_get_do_exhaustive_model_search := GetProcAddress(Libhandle, 'FLAC__stream_encoder_get_do_exhaustive_model_search');
    FLAC__stream_encoder_get_min_residual_partition_order := GetProcAddress(Libhandle, 'FLAC__stream_encoder_get_min_residual_partition_order');
    FLAC__stream_encoder_get_max_residual_partition_order := GetProcAddress(Libhandle, 'FLAC__stream_encoder_get_max_residual_partition_order');
  // FLAC__stream_encoder_get_rice_parameter_search_dist    := GetProcAddress(Libhandle, 'FLAC__stream_encoder_get_rice_parameter_search_dist');
    FLAC__stream_encoder_get_total_samples_estimate := GetProcAddress(Libhandle, 'FLAC__stream_encoder_get_total_samples_estimate');
    FLAC__stream_encoder_finish := GetProcAddress(Libhandle, 'FLAC__stream_encoder_finish');
    FLAC__stream_encoder_process := GetProcAddress(Libhandle, 'FLAC__stream_encoder_process');
    FLAC__stream_encoder_process_interleaved := GetProcAddress(Libhandle, 'FLAC__stream_encoder_process_interleaved');

    FLAC__stream_decoder_new := GetProcAddress(Libhandle, 'FLAC__stream_decoder_new');
    FLAC__stream_decoder_delete := GetProcAddress(Libhandle, 'FLAC__stream_decoder_delete');
    FLAC__stream_decoder_init_stream := GetProcAddress(Libhandle, 'FLAC__stream_decoder_init_stream');
    FLAC__stream_decoder_set_md5_checking := GetProcAddress(Libhandle, 'FLAC__stream_decoder_set_md5_checking');
    FLAC__stream_decoder_set_metadata_respond := GetProcAddress(Libhandle, 'FLAC__stream_decoder_set_metadata_respond');
    FLAC__stream_decoder_set_metadata_respond_application := GetProcAddress(Libhandle, 'FLAC__stream_decoder_set_metadata_respond_application');
    FLAC__stream_decoder_set_metadata_respond_all := GetProcAddress(Libhandle, 'FLAC__stream_decoder_set_metadata_respond_all');
    FLAC__stream_decoder_set_metadata_ignore := GetProcAddress(Libhandle, 'FLAC__stream_decoder_set_metadata_ignore');
    FLAC__stream_decoder_set_metadata_ignore_application := GetProcAddress(Libhandle, 'FLAC__stream_decoder_set_metadata_ignore_application');
    FLAC__stream_decoder_set_metadata_ignore_all := GetProcAddress(Libhandle, 'FLAC__stream_decoder_set_metadata_ignore_all');
    FLAC__stream_decoder_get_state := GetProcAddress(Libhandle, 'FLAC__stream_decoder_get_state');
    FLAC__stream_decoder_get_stream_decoder_state := GetProcAddress(Libhandle, 'FLAC__stream_decoder_get_stream_decoder_state');
    FLAC__stream_decoder_get_resolved_state_string := GetProcAddress(Libhandle, 'FLAC__stream_decoder_get_resolved_state_string');
    FLAC__stream_decoder_get_md5_checking := GetProcAddress(Libhandle, 'FLAC__stream_decoder_get_md5_checking');
    FLAC__stream_decoder_get_channels := GetProcAddress(Libhandle, 'FLAC__stream_decoder_get_channels');
    FLAC__stream_decoder_get_channel_assignment := GetProcAddress(Libhandle, 'FLAC__stream_decoder_get_channel_assignment');
    FLAC__stream_decoder_get_bits_per_sample := GetProcAddress(Libhandle, 'FLAC__stream_decoder_get_bits_per_sample');
    FLAC__stream_decoder_get_sample_rate := GetProcAddress(Libhandle, 'FLAC__stream_decoder_get_sample_rate');
    FLAC__stream_decoder_get_blocksize := GetProcAddress(Libhandle, 'FLAC__stream_decoder_get_blocksize');
    FLAC__stream_decoder_get_decode_position := GetProcAddress(Libhandle, 'FLAC__stream_decoder_get_decode_position');
    FLAC__stream_decoder_finish := GetProcAddress(Libhandle, 'FLAC__stream_decoder_finish');
    FLAC__stream_decoder_flush := GetProcAddress(Libhandle, 'FLAC__stream_decoder_flush');
    FLAC__stream_decoder_reset := GetProcAddress(Libhandle, 'FLAC__stream_decoder_reset');
    FLAC__stream_decoder_process_single := GetProcAddress(Libhandle, 'FLAC__stream_decoder_process_single');
    FLAC__stream_decoder_process_until_end_of_metadata := GetProcAddress(Libhandle, 'FLAC__stream_decoder_process_until_end_of_metadata');
    FLAC__stream_decoder_process_until_end_of_stream := GetProcAddress(Libhandle, 'FLAC__stream_decoder_process_until_end_of_stream');
    FLAC__stream_decoder_seek_absolute := GetProcAddress(Libhandle, 'FLAC__stream_decoder_seek_absolute');

  end;

finalization

  if Libhandle <> 0 then FreeLibrary(Libhandle);

 {$ENDIF}

 {$IFDEF LINUX}

var
  Libhandle : Pointer;

{$IFDEF SEARCH_LIBS}
  Path : String;
{$ENDIF}

initialization

{$IFDEF SEARCH_LIBS}

  Libhandle := nil;
  Path := FindLibs(LibFLACPath);
  if Path <> '' then Libhandle := dlopen(@Path[1], RTLD_NOW or RTLD_GLOBAL);

{$ELSE}

  Libhandle := dlopen(LibFLACPath, RTLD_NOW or RTLD_GLOBAL);

{$ENDIF}

  if Libhandle <> nil then
  begin
    LibFLACLoaded := True;

    FLAC__stream_encoder_new := dlsym(Libhandle, 'FLAC__stream_encoder_new');
    FLAC__stream_encoder_delete := dlsym(Libhandle, 'FLAC__stream_encoder_delete');
    FLAC__stream_encoder_set_verify := dlsym(Libhandle, 'FLAC__stream_encoder_set_verify');
    FLAC__stream_encoder_set_streamable_subset := dlsym(Libhandle, 'FLAC__stream_encoder_set_streamable_subset');
    FLAC__stream_encoder_set_do_mid_side_stereo := dlsym(Libhandle, 'FLAC__stream_encoder_set_do_mid_side_stereo');
    FLAC__stream_encoder_set_loose_mid_side_stereo := dlsym(Libhandle, 'FLAC__stream_encoder_set_loose_mid_side_stereo');
    FLAC__stream_encoder_set_channels := dlsym(Libhandle, 'FLAC__stream_encoder_set_channels');
    FLAC__stream_encoder_set_bits_per_sample := dlsym(Libhandle, 'FLAC__stream_encoder_set_bits_per_sample');
    FLAC__stream_encoder_set_sample_rate := dlsym(Libhandle, 'FLAC__stream_encoder_set_sample_rate');
    FLAC__stream_encoder_set_blocksize := dlsym(Libhandle, 'FLAC__stream_encoder_set_blocksize');
    FLAC__stream_encoder_set_max_lpc_order := dlsym(Libhandle, 'FLAC__stream_encoder_set_max_lpc_order');
    FLAC__stream_encoder_set_qlp_coeff_precision := dlsym(Libhandle, 'FLAC__stream_encoder_set_qlp_coeff_precision');
    FLAC__stream_encoder_set_do_qlp_coeff_prec_search := dlsym(Libhandle, 'FLAC__stream_encoder_set_do_qlp_coeff_prec_search');
    FLAC__stream_encoder_set_do_escape_coding := dlsym(Libhandle, 'FLAC__stream_encoder_set_do_escape_coding');
    FLAC__stream_encoder_set_do_exhaustive_model_search := dlsym(Libhandle, 'FLAC__stream_encoder_set_do_exhaustive_model_search');
    FLAC__stream_encoder_set_min_residual_partition_order := dlsym(Libhandle, 'FLAC__stream_encoder_set_min_residual_partition_order');
    FLAC__stream_encoder_set_max_residual_partition_order := dlsym(Libhandle, 'FLAC__stream_encoder_set_max_residual_partition_order');
  //    FLAC__stream_encoder_set_rice_parameter_search_dist := dlsym(Libhandle, 'FLAC__stream_encoder_set_rice_parameter_search_dist');
    FLAC__stream_encoder_set_total_samples_estimate := dlsym(Libhandle, 'FLAC__stream_encoder_set_total_samples_estimate');
    FLAC__stream_encoder_set_metadata := dlsym(Libhandle, 'FLAC__stream_encoder_set_metadata');
    FLAC__stream_encoder_set_client_data := dlsym(Libhandle, 'FLAC__stream_encoder_set_client_data');
    FLAC__stream_encoder_get_state := dlsym(Libhandle, 'FLAC__stream_encoder_get_state');
    FLAC__stream_encoder_get_stream_encoder_state := dlsym(Libhandle, 'FLAC__stream_encoder_get_stream_encoder_state');
    FLAC__stream_encoder_get_verify_decoder_state := dlsym(Libhandle, 'FLAC__stream_encoder_get_verify_decoder_state');
    FLAC__stream_encoder_get_resolved_state_string := dlsym(Libhandle, 'FLAC__stream_encoder_get_resolved_state_string');
    FLAC__stream_encoder_get_verify_decoder_error_stats := dlsym(Libhandle, 'FLAC__stream_encoder_get_verify_decoder_error_stats');
    FLAC__stream_encoder_get_verify := dlsym(Libhandle, 'FLAC__stream_encoder_get_verify');
    FLAC__stream_encoder_get_streamable_subset := dlsym(Libhandle, 'FLAC__stream_encoder_get_streamable_subset');
    FLAC__stream_encoder_get_do_mid_side_stereo := dlsym(Libhandle, 'FLAC__stream_encoder_get_do_mid_side_stereo');
    FLAC__stream_encoder_get_loose_mid_side_stereo := dlsym(Libhandle, 'FLAC__stream_encoder_get_loose_mid_side_stereo');
    FLAC__stream_encoder_get_channels := dlsym(Libhandle, 'FLAC__stream_encoder_get_channels');
    FLAC__stream_encoder_get_bits_per_sample := dlsym(Libhandle, 'FLAC__stream_encoder_get_bits_per_sample');
    FLAC__stream_encoder_get_sample_rate := dlsym(Libhandle, 'FLAC__stream_encoder_get_sample_rate');
    FLAC__stream_encoder_get_blocksize := dlsym(Libhandle, 'FLAC__stream_encoder_get_blocksize');
    FLAC__stream_encoder_get_max_lpc_order := dlsym(Libhandle, 'FLAC__stream_encoder_get_max_lpc_order');
    FLAC__stream_encoder_get_qlp_coeff_precision := dlsym(Libhandle, 'FLAC__stream_encoder_get_qlp_coeff_precision');
    FLAC__stream_encoder_get_do_qlp_coeff_prec_search := dlsym(Libhandle, 'FLAC__stream_encoder_get_do_qlp_coeff_prec_search');
    FLAC__stream_encoder_get_do_escape_coding := dlsym(Libhandle, 'FLAC__stream_encoder_get_do_escape_coding');
    FLAC__stream_encoder_get_do_exhaustive_model_search := dlsym(Libhandle, 'FLAC__stream_encoder_get_do_exhaustive_model_search');
    FLAC__stream_encoder_get_min_residual_partition_order := dlsym(Libhandle, 'FLAC__stream_encoder_get_min_residual_partition_order');
    FLAC__stream_encoder_get_max_residual_partition_order := dlsym(Libhandle, 'FLAC__stream_encoder_get_max_residual_partition_order');
  // FLAC__stream_encoder_get_rice_parameter_search_dist    := dlsym(Libhandle, 'FLAC__stream_encoder_get_rice_parameter_search_dist');
    FLAC__stream_encoder_get_total_samples_estimate := dlsym(Libhandle, 'FLAC__stream_encoder_get_total_samples_estimate');
    FLAC__stream_encoder_init := dlsym(Libhandle, 'FLAC__stream_encoder_init');
    FLAC__stream_encoder_finish := dlsym(Libhandle, 'FLAC__stream_encoder_finish');
    FLAC__stream_encoder_process := dlsym(Libhandle, 'FLAC__stream_encoder_process');
    FLAC__stream_encoder_process_interleaved := dlsym(Libhandle, 'FLAC__stream_encoder_process_interleaved');

    FLAC__stream_decoder_new := dlsym(Libhandle, 'FLAC__stream_decoder_new');
    FLAC__stream_decoder_delete := dlsym(Libhandle, 'FLAC__stream_decoder_delete');
    FLAC__stream_decoder_set_md5_checking := dlsym(Libhandle, 'FLAC__stream_decoder_set_md5_checking');
    FLAC__stream_decoder_set_client_data := dlsym(Libhandle, 'FLAC__stream_decoder_set_client_data');
    FLAC__stream_decoder_set_metadata_respond := dlsym(Libhandle, 'FLAC__stream_decoder_set_metadata_respond');
    FLAC__stream_decoder_set_metadata_respond_application := dlsym(Libhandle, 'FLAC__stream_decoder_set_metadata_respond_application');
    FLAC__stream_decoder_set_metadata_respond_all := dlsym(Libhandle, 'FLAC__stream_decoder_set_metadata_respond_all');
    FLAC__stream_decoder_set_metadata_ignore := dlsym(Libhandle, 'FLAC__stream_decoder_set_metadata_ignore');
    FLAC__stream_decoder_set_metadata_ignore_application := dlsym(Libhandle, 'FLAC__stream_decoder_set_metadata_ignore_application');
    FLAC__stream_decoder_set_metadata_ignore_all := dlsym(Libhandle, 'FLAC__stream_decoder_set_metadata_ignore_all');
    FLAC__stream_decoder_get_state := dlsym(Libhandle, 'FLAC__stream_decoder_get_state');
    FLAC__stream_decoder_get_stream_decoder_state := dlsym(Libhandle, 'FLAC__stream_decoder_get_stream_decoder_state');
    FLAC__stream_decoder_get_resolved_state_string := dlsym(Libhandle, 'FLAC__stream_decoder_get_resolved_state_string');
    FLAC__stream_decoder_get_md5_checking := dlsym(Libhandle, 'FLAC__stream_decoder_get_md5_checking');
    FLAC__stream_decoder_get_channels := dlsym(Libhandle, 'FLAC__stream_decoder_get_channels');
    FLAC__stream_decoder_get_channel_assignment := dlsym(Libhandle, 'FLAC__stream_decoder_get_channel_assignment');
    FLAC__stream_decoder_get_bits_per_sample := dlsym(Libhandle, 'FLAC__stream_decoder_get_bits_per_sample');
    FLAC__stream_decoder_get_sample_rate := dlsym(Libhandle, 'FLAC__stream_decoder_get_sample_rate');
    FLAC__stream_decoder_get_blocksize := dlsym(Libhandle, 'FLAC__stream_decoder_get_blocksize');
    FLAC__stream_decoder_get_decode_position := dlsym(Libhandle, 'FLAC__stream_decoder_get_decode_position');
    FLAC__stream_decoder_init := dlsym(Libhandle, 'FLAC__stream_decoder_init');
    FLAC__stream_decoder_finish := dlsym(Libhandle, 'FLAC__stream_decoder_finish');
    FLAC__stream_decoder_flush := dlsym(Libhandle, 'FLAC__stream_decoder_flush');
    FLAC__stream_decoder_reset := dlsym(Libhandle, 'FLAC__stream_decoder_reset');
    FLAC__stream_decoder_process_single := dlsym(Libhandle, 'FLAC__stream_decoder_process_single');
    FLAC__stream_decoder_process_until_end_of_metadata := dlsym(Libhandle, 'FLAC__stream_decoder_process_until_end_of_metadata');
    FLAC__stream_decoder_process_until_end_of_stream := dlsym(Libhandle, 'FLAC__stream_decoder_process_until_end_of_stream');
    FLAC__stream_decoder_seek_absolute := dlsym(Libhandle, 'FLAC__stream_decoder_seek_absolute');

  end;

finalization

  if Libhandle <> nil then dlclose(Libhandle);

  {$ENDIF}

end.
