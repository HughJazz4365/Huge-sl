pub const spirv_magic: u32 = 0x07230203;
pub const source_language: u32 = 0; //unknown

pub const Op = enum(u32) {
    //debug
    name = 5,
    member_name = 6,

    //annotation
    decorate = 71,
    member_decorate = 72,
    // decoration_group = 73,
    // group_decorate = 74,
    // group_member_decorate = 74,

    //extensions
    extension = 10,
    ext_inst_import = 11,
    ext_inst = 12,

    //settings
    memory_model = 14,
    entry_point = 15,
    execution_mode = 16,
    capability = 17,

    //type declarations
    type_void = 19,
    type_bool = 20,

    type_int = 21,
    type_float = 22,
    type_vector = 23,
    type_matrix = 24,

    type_image = 25,
    type_sampler = 26,
    type_sampled_image = 27,

    type_array = 28,
    type_runtime_array = 29,

    type_struct = 30,
    type_pointer = 32,
    type_function = 33,

    //constant creation
    const_true = 41,
    const_false = 42,
    const_scalar = 43,
    const_composite = 44,

    //memory instructions
    variable = 59,
    load = 61,
    store = 62,
    copy = 63, //replace load(a)->store(b,a) pattern
    copy_memory_sized = 64,
    access_chain = 65,
    ptr_access_chain = 67,

    //functions
    function_declaration = 54,
    function_parameter = 55,
    function_end = 56,
    function_call = 57,

    //images
    image_sample = 87,
    image_sample_lod = 88,
    image_fetch = 95, //get single texel from sampled image(not cube)
    image_gather = 96, //sampled image (2d or cube)
    image_read = 98,
    image_write = 99,
    image_unsampled = 100, //extract image from sampled??
    //sparse image instructions

    //conversion
    cast_float_to_uint = 109,
    cast_float_to_int = 110,
    cast_int_to_float = 111,
    cast_uint_to_float = 112,
    cast_uint = 113,
    cast_int = 114,
    cast_float = 115,

    cast_ptr_to_uint = 117,
    cast_uint_to_ptr = 120,
    bitcast = 124,

    quantize_to_f16 = 116,

    //composite
    vector_extract_dynamic = 77,
    vector_insert_dynamic = 78,
    vector_shuffle = 79,
    composite_construct = 80,
    composite_extract = 81,
    composite_insert = 82,
    copy_object = 83,
    transpose = 84,

    //arithmetic
    s_negate = 126,
    f_negate = 127,

    i_add = 128,
    f_add = 129,

    i_sub = 130,
    f_sub = 131,

    i_mul = 132,
    f_mul = 133,

    u_div = 134,
    s_div = 135,
    f_div = 136,

    u_mod = 137,
    s_rem = 138,
    s_mod = 139,
    f_rem = 140,
    f_mod = 141,

    vec_x_scalar = 142,
    matrix_x_scalar = 143,
    vector_x_matrix = 144,
    matrix_x_vector = 145,
    matrix_x_matrix = 146,
    outer_product = 147,
    dot = 148,

    //bit
    bitshift_right = 194,
    bitshift_legth = 196,
    bitshift_right_arithmetic = 195,
    bitwise_or = 197,
    bitwise_xor = 198,
    bitwise_and = 199,
    bitwise_not = 200,

    bitfield_insert = 201,

    bitfield_extract_int = 202,
    bitfield_extract_uint = 203,

    bit_reverse = 204,
    popcnt = 205,

    //relational logical
    any = 154,
    all = 155,

    logical_equal = 164,
    logical_not_equal = 165,
    logical_or = 166,
    logical_and = 167,
    logical_not = 168,

    select = 169,

    i_equal = 170,
    i_not_equal = 171,

    u_greater = 172,
    s_greater = 173,
    u_greater_equal = 174,
    s_greater_equal = 175,

    u_less = 176,
    s_less = 177,
    u_less_equal = 178,
    s_less_equal = 179,

    f_ord_equal = 180,
    f_unord_not_equal = 183,
    f_ord_less = 184,
    f_ord_less_equal = 188,
    f_ord_greater = 186,
    f_ord_greater_equal = 190,

    //derivative

    d_pdx = 207,
    d_pdy = 208,
    f_width = 209,

    //control-flow
    phi = 245,
    loop_merge = 246,
    selection_merge = 247,
    label = 248,
    branch = 249,
    branch_conditional = 250,
    @"switch" = 251,

    discard = 252,
    @"return" = 253,
    return_value = 254,

    //atomic
    //barrier
    //group
};
pub const ExecutionModel = enum(u32) {
    vertex = 0,
    fragment = 4,
    compute = 5,

    task_nv = 5267, //?? only nvidia stinky
    mesh_nv = 5268,
};

pub const ExecutionMode = enum(u32) {
    workgroup_size = 17,

    early_fragment_tests = 9, //auto
    depth_replacing = 12, //auto
    depth_greater = 14, //auto not rly important
    depth_less = 15, //auto

    origin_upper_left = 7,
    origin_lower_left = 8, //bad idea
};
//for opLoad, opStore, etc:
//This access has a known alignment,
//provided as a literal in the next operand.
pub const aligned_memory_access: u32 = 0x2;

pub const memory_model: u32 = 1; //glsl

pub const AddressingModel = enum(u32) {
    logical = 0,
    physical_storage_buffer = 5348,
};
pub const StorageClass = enum(u32) {
    input = 1,
    output = 3,
    push_constant = 9,

    function = 7, //local var
    private = 6, //global var

    workgroup = 4,
    atomic_counter = 10, //??

    physical_storage_buffer = 5349,
    storage_buffer = 12, //fallback?
};

pub const ImageOperand = enum(u32) {
    bias = 0x1,
    lod = 0x2,
    min_lod = 0x80,
}; //idk about other ones
pub const unknown_image_format: u32 = 0;
pub const ImageDimensionality = enum(u32) {
    @"1d" = 0,
    @"2d" = 1,
    @"3d" = 2,
    cube = 3,
};

pub const Builtin = enum(u32) {
    //vertex
    point_size = 1, //input
    clip_distance = 3,
    cull_distance = 4,
    vertex_id = 5,
    // vertex_index = 42,
    instance_id = 6,
    position = 0, //output

    //fragment
    frag_coord = 15, //input
    front_facing = 17,
    frag_depth = 22, //output

    //compute
    workgroup_count = 24, //input
    workgroup_id = 26,
    local_invocation_id = 27,
    // local_invocation_id = 29,
    global_invocation_id = 28,
};

pub const Decoration = enum(u32) {
    relaxed_precision = 0,

    row_major = 4,
    col_major = 5,
    array_stride = 6,
    matrix_stride = 7,
    builtin = 11,

    linear = 13, //noperspective
    flat = 14,

    not_aliased = 19,
    aliased = 20,

    readonly = 24, //cannot be used on device pointers
    writeonly = 25,

    location = 30,
    binding = 33,
    descriptor_set = 34,

    offset = 35,
};

pub const Capability = enum(u32) {
    matrix = 0,
    shader = 1,

    physical_storage_buffer_addresses = 5347, //bda

    float16 = 9, //VK_KHR_shader_float16_int8
    float16_buffer = 8,
    float64 = 10,

    int8 = 39, //VK_KHR_shader_float16_int8
    int8_storage_buffer_access = 4448, //SPV_KHR_8bit_storage
    int8_push_constant = 4450, //SPV_KHR_8bit_storage

    int16 = 22,
    int16_storage_buffer_access = 4433, //SPV_KHR_8bit_storage
    int16_push_constant = 4435, //SPV_KHR_8bit_storage
    int16_input_output = 4436,

    int64 = 11,
    int64_atomics = 12,
    atomic_storage = 21,

    //gather with dynamic offsets
    image_gather_extended = 25,

    sampled_image_dynamic_indexing = 29,
    storage_image_dynamic_indexing = 31,
    storage_buffer_dynamic_indexing = 30,

    clip_distance = 32,
    cull_distance = 33,
};

pub const glsl_std_ext_inst_set_name =
    "GLSL.std.450";
pub const physical_storage_buffer_ext_name =
    "SPV_KHR_physical_storage_buffer";
pub const @"8bit_storage_ext_name" =
    "SPV_KHR_8bit_storage";
pub const @"16bit_storage_ext_name" =
    "SPV_KHR_16bit_storage";
