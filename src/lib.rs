use proc_macro::TokenStream;
use proc_macro2::Span;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::{
    Ident, LitBool, LitChar, LitInt, LitStr, Token, bracketed,
    ext::IdentExt,
    parenthesized,
    parse::{Parse, ParseStream, Result},
    parse_macro_input,
};
const IMPOSSIBLE_ERROR: &str = "Inner Parsing Error occured. Please be so kind and open a new Issue at https://github.com/revellan/revparse/issues called IMPOSSIBLE ERROR";
enum ArgSetting {
    ExecName(LitStr),
    Pos(LitStr),
    PosHelp(LitStr),
    MinPos(LitInt),
    MaxPos(LitInt),
    InfinitePos(LitBool),
    FunctionName(Ident),
    ModName(Ident),
}
enum ArgKind {
    Both {
        long: Ident,
        short: LitChar,
        val: LitStr,
        help: LitStr,
    },
    NoShort {
        long: Ident,
        val: LitStr,
        help: LitStr,
    },
    NoVal {
        long: Ident,
        short: LitChar,
        help: LitStr,
    },
    Neither {
        long: Ident,
        help: LitStr,
    },
    Setting(ArgSetting),
}
impl ArgKind {
    fn short_name(&self) -> Option<char> {
        match self {
            Self::Both { short, .. } => Some(short.value()),
            Self::NoVal { short, .. } => Some(short.value()),
            _ => None,
        }
    }
    fn long_name(&self) -> String {
        match self {
            Self::Both { long, .. } => long.to_string(),
            Self::NoVal { long, .. } => long.to_string(),
            Self::NoShort { long, .. } => long.to_string(),
            Self::Neither { long, .. } => long.to_string(),
            _ => panic!("{}", IMPOSSIBLE_ERROR),
        }
    }
    fn take_value(&self) -> Option<String> {
        match self {
            Self::Both { val, .. } => Some(val.value()),
            Self::NoShort { val, .. } => Some(val.value()),
            _ => None,
        }
    }
    fn help_msg(&self) -> String {
        match self {
            Self::Both { help, .. } => help.value(),
            Self::NoVal { help, .. } => help.value(),
            Self::NoShort { help, .. } => help.value(),
            Self::Neither { help, .. } => help.value(),
            _ => panic!("{}", IMPOSSIBLE_ERROR),
        }
    }
}
impl Parse for ArgKind {
    fn parse(input: ParseStream) -> Result<Self> {
        let ident: Ident = input.call(Ident::parse_any)?;
        if input.peek(Token![,]) {
            input.parse::<Token![,]>()?;
            if input.peek(LitStr) {
                // NoShort and Neither
                let help: LitStr = input.parse()?;
                if input.peek(Token![,]) {
                    // NoShort
                    input.parse::<Token![,]>()?;
                    let val: LitStr = input.parse()?;
                    Ok(ArgKind::NoShort {
                        long: ident,
                        val,
                        help,
                    })
                } else {
                    // Neither
                    Ok(ArgKind::Neither { long: ident, help })
                }
            } else {
                // Both, NoVal
                let short: LitChar = input.parse()?;
                input.parse::<Token![,]>()?;
                let help: LitStr = input.parse()?;
                if input.peek(Token![,]) {
                    // Both
                    input.parse::<Token![,]>()?;
                    let val: LitStr = input.parse()?;
                    Ok(ArgKind::Both {
                        long: ident,
                        short,
                        val,
                        help,
                    })
                } else {
                    // Noval
                    Ok(ArgKind::NoVal {
                        long: ident,
                        short,
                        help,
                    })
                }
            }
        } else {
            input.parse::<Token![=>]>()?;
            use ArgKind::Setting as ArgSet;
            use ArgSetting::*;
            match ident.to_string().as_ref() {
                "ExecName" => Ok(ArgSet(ExecName(input.parse()?))),
                "Pos" => Ok(ArgSet(Pos(input.parse()?))),
                "PosMin" => Ok(ArgSet(MinPos(input.parse()?))),
                "PosMax" => Ok(ArgSet(MaxPos(input.parse()?))),
                "PosInfinite" => Ok(ArgSet(InfinitePos(input.parse()?))),
                "PosHelp" => Ok(ArgSet(PosHelp(input.parse()?))),
                "FunctionName" => {
                    let function_name = Ok(ArgSet(FunctionName(input.parse()?)));
                    let _content;
                    parenthesized!(_content in input);
                    function_name
                }
                "ModName" => Ok(ArgSet(ModName(input.parse()?))),
                ident_str => Err(syn::Error::new(
                    ident.span(),
                    format!("`{}` is not a valid setting!", ident_str),
                )),
            }
        }
    }
}
struct RevparseInt {
    exec_name: String,
    usage: String,
    help: String,
    min_pos_args: u64,
    custom_max_pos_args: bool,
    max_pos_args: u64,
    pos_arg_help: Option<String>,
    infinite_pos_args: bool,
    args: Vec<ArgKind>,
    pres_pos_args: Vec<String>,
    function_name: Ident,
    mod_name: Ident,
}
impl RevparseInt {
    fn match_setting(&mut self, setting: ArgSetting) {
        use ArgSetting::*;
        match setting {
            ExecName(name) => self.exec_name = name.value(),
            Pos(pos_arg) => self.pres_pos_args.push(pos_arg.value()),
            PosHelp(pos_help) => self.pos_arg_help = Some(pos_help.value()),
            MinPos(min_pos) => {
                self.min_pos_args = min_pos
                    .base10_parse()
                    .expect("PosMin should be of type u64")
            }
            MaxPos(max_pos) => {
                self.max_pos_args = max_pos
                    .base10_parse()
                    .expect("PosMin should be of type u64");
                self.custom_max_pos_args = true;
            }
            InfinitePos(boolean) => self.infinite_pos_args = boolean.value(),
            FunctionName(function_name) => self.function_name = function_name,
            ModName(mod_name) => self.mod_name = mod_name,
        }
    }
    fn create_help(&mut self) {
        self.help = match &self.pos_arg_help {
            Some(msg) => format!("\n{}\n\nOptions:\n", msg),
            None => String::from("\n\nOptions:\n"),
        };
        self.usage = String::from(format!("Usage: {} [OPTION]...", self.exec_name));
        if self.pres_pos_args.len() != 0 {
            for i in &self.pres_pos_args {
                self.usage.push_str(&format!(" {}", i.to_uppercase()))
            }
        }
        self.help
            .push_str("  -h, --help                display this help text and exit\n");
        for arg in &self.args {
            let mut length: usize; //28 chars between help_msg and the beginning of the line
            let i = arg.long_name().replace('_', "-");
            match arg.short_name() {
                Some(sn) => {
                    self.help.push_str(format!("  -{}, --{}", sn, i).as_str());
                    length = 20 - i.len();
                }
                None => {
                    self.help.push_str(format!("  --{}", i).as_str());
                    length = 24 - i.len();
                }
            }
            if let Some(value) = arg.take_value() {
                length -= value.len() + 1;
                self.help
                    .push_str(format!("={}", value.to_uppercase()).as_str());
            }
            if length <= 2 {
                self.help.push_str("  ");
            } else {
                self.help.push_str(&" ".repeat(length));
            }
            self.help.push_str(&arg.help_msg());
            self.help.push('\n');
        }
    }
    fn mk_add_pos_internal(&self) -> (TokenStream2, TokenStream2, TokenStream2, TokenStream2) {
        let RevparseInt {
            min_pos_args,
            max_pos_args,
            infinite_pos_args,
            ..
        } = self;
        let decide_type = |x: &u64, field_name: Ident| {
            if *x <= std::u8::MAX as u64 {
                quote! {
                    #field_name: RefCell<u8>,
                }
            } else if *x <= std::u16::MAX as u64 {
                quote! {
                    #field_name: RefCell<u16>,
                }
            } else if *x <= std::u32::MAX as u64 {
                quote! {
                    #field_name: RefCell<u32>,
                }
            } else {
                quote! {
                    #field_name: RefCell<u64>,
                }
            }
        };
        let init_number_as_lowest_type = |x: &u64, field_name: Ident| {
            if *x <= std::u8::MAX as u64 {
                let num = *x as u8;
                quote! {
                    #field_name: RefCell::new(#num),
                }
            } else if *x <= std::u16::MAX as u64 {
                let num = *x as u16;
                quote! {
                    #field_name: RefCell::new(#num),
                }
            } else if *x <= std::u32::MAX as u64 {
                let num = *x as u32;
                quote! {
                    #field_name: RefCell::new(#num),
                }
            } else {
                let num = *x;
                quote! {
                    #field_name: RefCell::new(#num),
                }
            }
        };
        let min_ident = Ident::new("min_pos_args", Span::call_site());
        let max_ident = Ident::new("max_pos_args", Span::call_site());
        let min_pos_args_field = decide_type(min_pos_args, min_ident.clone());
        let max_pos_args_field = decide_type(max_pos_args, max_ident.clone());
        let raw_add = quote! {
            self.pos_args.borrow_mut().as_mut().unwrap().push(arg);
        };
        let pos_struct_field = quote! {
            pos_args: RefCell<Option<Vec<String>>>,
        };
        let pos_init = quote! {
            pos_args: RefCell::new(Some(Vec::new())),
        };
        let min_init = init_number_as_lowest_type(min_pos_args, min_ident);
        let max_init = init_number_as_lowest_type(max_pos_args, max_ident);
        let main_function;
        let declare_fields;
        let init_fields;
        let control_function;
        if *infinite_pos_args && *min_pos_args == 0 {
            main_function = quote! {
                fn add_pos_internal(&self, arg: String) {
                    #raw_add
                }
            };
            declare_fields = quote! {
                #pos_struct_field
            };
            init_fields = quote! {
                #pos_init
            };
            control_function = quote! {
                fn check_pos(&self) {
                    return;
                }
            };
        } else if *infinite_pos_args {
            main_function = quote! {
                fn add_pos_internal(&self, arg: String) {
                    if *self.min_pos_args.borrow() != 0 {
                        *self.min_pos_args.borrow_mut() -= 1;
                    }
                    #raw_add
                }
            };
            declare_fields = quote! {
                #pos_struct_field
                #min_pos_args_field
            };
            init_fields = quote! {
                #pos_init
                #min_init
            };
            control_function = quote! {
                fn check_pos(&self) {
                    if *self.min_pos_args.borrow() != 0 {
                        print_usage();
                        exit(1);
                    }
                }
            };
        } else if *min_pos_args == 0 {
            main_function = quote! {
                fn add_pos_internal(&self, arg: String) {
                    if *self.max_pos_args.borrow() == 0 {
                        err_extra_operand(&arg);
                        exit(1);
                    }
                    *self.max_pos_args.borrow_mut() -= 1;
                    #raw_add
                }
            };
            declare_fields = quote! {
                #pos_struct_field
                #max_pos_args_field
            };
            init_fields = quote! {
                #pos_init
                #max_init
            };
            control_function = quote! {
                fn check_pos(&self) {
                    return;
                }
            }
        } else {
            main_function = quote! {
                fn add_pos_internal(&self, arg: String) {
                    if *self.max_pos_args.borrow() == 0 {
                        err_extra_operand(&arg);
                        exit(1);
                    }
                    *self.max_pos_args.borrow_mut() -= 1;
                    if *self.min_pos_args.borrow() != 0 {
                        *self.min_pos_args.borrow_mut() -= 1;
                    }
                    #raw_add
                }
            };
            declare_fields = quote! {
                #pos_struct_field
                #min_pos_args_field
                #max_pos_args_field
            };
            init_fields = quote! {
                #pos_init
                #min_init
                #max_init
            };
            control_function = quote! {
                fn check_pos(&self) {
                    if *self.min_pos_args.borrow() != 0 {
                        print_usage();
                        exit(1);
                    }
                }
            };
        }
        (main_function, declare_fields, init_fields, control_function)
    }
    fn dec_mut_struct_fields(&self) -> TokenStream2 {
        self.args
            .iter()
            .map(|flag| {
                use ArgKind::*;
                match flag {
                    Both { long, .. } | NoShort { long, .. } => quote! {
                        #long: ArgVal,
                    },
                    NoVal { long, .. } | Neither { long, .. } => quote! {
                        #long: ArgBool,
                    },
                    _ => panic!("{}", IMPOSSIBLE_ERROR),
                }
            })
            .collect()
    }
    fn dec_inner_struct_fields(&self, dec_pos_fields: Option<TokenStream2>) -> TokenStream2 {
        let dec_fields = match dec_pos_fields {
            Some(fields) => quote! {#fields},
            None => TokenStream2::new(),
        };
        quote! {
            hashmap_long: HashMap<&'static str, &'a dyn Argument>,
            hashmap_short: HashMap<char, &'a dyn Argument>,
            #dec_fields
        }
    }
    fn init_inner_struct_fields(&self, init_pos_fields: Option<TokenStream2>) -> TokenStream2 {
        match init_pos_fields {
            Some(tokens) => quote! {
                hashmap_long: HashMap::new(),
                hashmap_short: HashMap::new(),
                #tokens
            },
            None => quote! {
                hashmap_long: HashMap::new(),
                hashmap_short: HashMap::new(),
            },
        }
    }
    fn init_hashmap(&self) -> TokenStream2 {
        (&self)
            .args
            .iter()
            .map(|i| {
                use ArgKind::*;
                let (long, short) = match i {
                    Both { long, short, .. } | NoVal { long, short, .. } => (long, Some(short)),
                    NoShort { long, .. } | Neither { long, .. } => (long, None),
                    _ => panic!("{}", IMPOSSIBLE_ERROR),
                };
                let long_str = format!("--{}", long.to_string().replace('_', "-"));
                if let Some(short) = short {
                    quote! {
                        inner.hashmap_long.insert(#long_str, &mut_rvp.#long);
                        inner.hashmap_short.insert(#short, &mut_rvp.#long);
                    }
                } else {
                    quote! {
                        inner.hashmap_long.insert(#long_str, &mut_rvp.#long);
                    }
                }
            })
            .collect()
    }
    fn init_mut_struct_fields(&self) -> TokenStream2 {
        (&self)
            .args
            .iter()
            .map(|i| {
                use ArgKind::*;
                match i {
                    Both { long, short, .. } => {
                        let long_str = format!("--{}", long.to_string().replace('_', "-"));
                        quote! {
                            #long: ArgVal { value: RefCell::new(None), kind: ArgKind::Short(#long_str, #short)},
                        }
                    },
                    NoVal { long, short, .. } => {
                        let long_str = format!("--{}", long.to_string().replace('_', "-"));
                        quote! {
                            #long: ArgBool { value: RefCell::new(false), kind: ArgKind::Short(#long_str, #short)},
                        }
                    },
                    NoShort { long, .. } => {
                        let long_str = format!("--{}", long.to_string().replace('_', "-"));
                        quote! {
                            #long: ArgVal { value: RefCell::new(None), kind: ArgKind::NoShort(#long_str)},
                        }
                    },
                    Neither { long, .. } => {
                        let long_str = format!("--{}", long.to_string().replace('_', "-"));
                        quote! {
                            #long: ArgBool { value: RefCell::new(false), kind: ArgKind::NoShort(#long_str)},
                        }
                    },
                    _ => panic!("{}", IMPOSSIBLE_ERROR),
                }
            })
            .collect()
    }
    fn dec_pub_struct_fields(&self) -> TokenStream2 {
        self.args
            .iter()
            .map(|i| {
                use ArgKind::*;
                match i {
                    Both { long, .. } | NoShort { long, .. } => {
                        quote! {
                            pub #long: Option<String>,
                        }
                    }
                    NoVal { long, .. } | Neither { long, .. } => {
                        quote! {
                            pub #long: bool,
                        }
                    }
                    _ => panic!("{}", IMPOSSIBLE_ERROR),
                }
            })
            .collect()
    }
    fn init_pub_struct_fields(&self) -> TokenStream2 {
        self.args
            .iter()
            .map(|i| {
                use ArgKind::*;
                match i {
                    Both { long, .. } | NoShort { long, .. } => {
                        quote! {
                            #long: mut_rvp.#long.value.borrow_mut().take(),
                        }
                    }
                    NoVal { long, .. } | Neither { long, .. } => {
                        quote! {
                            #long: *mut_rvp.#long.value.borrow(),
                        }
                    }
                    _ => panic!("{}", IMPOSSIBLE_ERROR),
                }
            })
            .collect()
    }
    fn mk_module(&self) -> TokenStream2 {
        let positional_logic =
            self.pres_pos_args.len() != 0 || self.custom_max_pos_args || self.infinite_pos_args;
        let RevparseInt {
            help,
            usage,
            exec_name,
            ..
        } = self;
        let dec_mut_struct_fields = self.dec_mut_struct_fields();
        let init_mut_struct_fields = self.init_mut_struct_fields();
        let dec_pub_struct_fields = self.dec_pub_struct_fields();
        let init_pub_struct_fields = self.init_pub_struct_fields();
        let dec_inner_struct_fields;
        let init_inner_struct_fields;
        let pos_control_function;
        let init_hashmap = self.init_hashmap();
        let add_pos_internal;
        let parser_logic;
        let get_pos_args_function;
        let rvp_pos_args_field_init;
        let rvp_pos_args_field_dec;
        if positional_logic {
            let (main_function, declare_fields, init_fields, control_function) =
                self.mk_add_pos_internal();
            dec_inner_struct_fields = self.dec_inner_struct_fields(Some(declare_fields));
            init_inner_struct_fields = self.init_inner_struct_fields(Some(init_fields));
            pos_control_function = control_function;
            add_pos_internal = main_function;
            get_pos_args_function = quote! {
                pub fn get_pos_args(&mut self) -> Vec<String> {
                    self._pos_args.take().unwrap_or_else(|| panic!("get_pos_args() was called before and moved the Vec<String>. Consider binding it to a variable."))
                }
            };
            rvp_pos_args_field_init = quote! {
                _pos_args: inner.pos_args.borrow_mut().take(),
            };
            rvp_pos_args_field_dec = quote! {
                _pos_args: Option<Vec<String>>,
            };
            parser_logic = quote! {
                let mut next_is_val: Option<(&dyn Argument, bool)> = None;
                let mut next_is_pos = false;
                for e_arg in args.skip(1) {
                    if let Some((arg_struct, _)) = next_is_val.take() {
                        (*arg_struct).insert(e_arg);
                    } else if next_is_pos {
                        next_is_pos = false;
                        inner.add_pos_internal(e_arg);
                    } else if e_arg == "--help" || e_arg == "-h" {
                        print_help();
                        exit(0);
                    } else if e_arg == "--" {
                        next_is_pos = true;
                    } else if e_arg.starts_with("--") {
                        match e_arg
                            .split_once('=')
                            .map(|(arg_name, val)| (arg_name, val.to_string()))
                        {
                            Some((arg_name, val)) => {
                                let arg_struct = inner.get_str(&arg_name);
                                if (*arg_struct).take_val() {
                                    (*arg_struct).insert(val);
                                } else {
                                    err_no_val_allowed(&arg_name);
                                    exit(1);
                                }
                            },
                            None => {
                                let arg_struct = inner.get_str(&e_arg);
                                if (*arg_struct).take_val() {
                                    next_is_val = Some((arg_struct, true));
                                } else {
                                    (*arg_struct).was_called();
                                }
                            },
                        }
                    } else if e_arg.starts_with('-') {
                        let mut rest_is_val: Option<&dyn Argument> = None;
                        let mut value = String::new();
                        for e_char in e_arg.chars().skip(1) {
                            if rest_is_val.is_some() {
                                value.push(e_char);
                            } else {
                                if e_char == 'h' {
                                    print_help();
                                    exit(0);
                                }
                                let arg_struct = inner.get_char(e_char);
                                if (*arg_struct).take_val() {
                                    rest_is_val = Some(arg_struct);
                                } else {
                                    (*arg_struct).was_called();
                                }
                            }
                        }
                        if let Some(arg_struct) = rest_is_val.take() {
                            if value.len() == 0 {
                                next_is_val = Some((arg_struct, false));
                            } else {
                                (*arg_struct).insert(value);
                            }
                        }
                    } else {
                        inner.add_pos_internal(e_arg);
                    }
                }
                if let Some((arg_struct, is_long)) = next_is_val.take() {
                    if is_long {
                        err_opt_requires_arg((*arg_struct).long());
                        exit(1);
                    } else {
                        err_short_opt_requires_arg((*arg_struct).short());
                        exit(1);
                    }
                }
                inner.check_pos();
            };
        } else {
            dec_inner_struct_fields = self.dec_inner_struct_fields(None);
            init_inner_struct_fields = self.init_inner_struct_fields(None);
            get_pos_args_function = TokenStream2::new();
            pos_control_function = TokenStream2::new();
            add_pos_internal = TokenStream2::new();
            rvp_pos_args_field_init = TokenStream2::new();
            rvp_pos_args_field_dec = TokenStream2::new();
            parser_logic = quote! {
                let mut next_is_val: Option<(&dyn Argument, bool)> = None;
                for e_arg in args.skip(1) {
                    if let Some((arg_struct, _)) = next_is_val.take() {
                        (*arg_struct).insert(e_arg);
                    } else if e_arg == "--help" || e_arg == "-h" {
                        print_help();
                        exit(0);
                    } else if e_arg == "--" {
                        err_arg_does_not_exist("--");
                        exit(1);
                    } else if e_arg.starts_with("--") {
                        match e_arg
                            .split_once('=')
                            .map(|(arg_name, val)| (arg_name, val.to_string()))
                        {
                            Some((arg_name, val)) => {
                                let arg_struct = inner.get_str(&arg_name);
                                if (*arg_struct).take_val() {
                                    (*arg_struct).insert(val);
                                } else {
                                    err_no_val_allowed(&arg_name);
                                    exit(1);
                                }
                            },
                            None => {
                                let arg_struct = inner.get_str(&e_arg);
                                if (*arg_struct).take_val() {
                                    next_is_val = Some((arg_struct, true));
                                } else {
                                    (*arg_struct).was_called();
                                }
                            },
                        }
                    } else if e_arg.starts_with('-') {
                        let mut rest_is_val: Option<&dyn Argument> = None;
                        let mut value = String::new();
                        for e_char in e_arg.chars().skip(1) {
                            if rest_is_val.is_some() {
                                value.push(e_char);
                            } else {
                                if e_char == 'h' {
                                    print_help();
                                    exit(0);
                                }
                                let arg_struct = inner.get_char(e_char);
                                if (*arg_struct).take_val() {
                                    rest_is_val = Some(arg_struct);
                                } else {
                                    (*arg_struct).was_called();
                                }
                            }
                        }
                        if let Some(arg_struct) = rest_is_val.take() {
                            if value.len() == 0 {
                                next_is_val = Some((arg_struct, false));
                            } else {
                                (*arg_struct).insert(value);
                            }
                        }
                    } else {
                        err_arg_does_not_exist(&e_arg);
                        exit(1);
                    }
                }
                if let Some((arg_struct, is_long)) = next_is_val.take() {
                    if is_long {
                        err_opt_requires_arg((*arg_struct).long());
                        exit(1);
                    } else {
                        err_short_opt_requires_arg((*arg_struct).short());
                        exit(1);
                    }
                }
            };
        }
        quote! {
            #[cfg(test)]
            fn exit(code: i32) -> ! {
                panic!("Exit code: {}", code);
            }
            #[cfg(not(test))]
            fn exit(code: i32) -> ! {
                std::process::exit(code);
            }
            fn err_arg_does_not_exist(arg: &str) {
                if arg == "--help" {
                    err_no_val_allowed(arg);
                } else {
                    eprintln!(
                        "{}: unrecognized option '{}'\n{}\nTry '{} --help' for more information.",
                        #exec_name,
                        arg,
                        USAGE,
                        #exec_name
                    );
                }
            }
            fn err_short_arg_does_not_exist(ch: char) {
                eprintln!(
                    "{}: invalid option -- '{}'\n{}\nTry '{} --help' for more information.",
                    #exec_name,
                    ch,
                    USAGE,
                    #exec_name,
                );
            }
            fn err_extra_operand(arg: &str) {
                eprintln!(
                    "{}: extra operand '{}'\n{}\nTry '{} --help' for more information.",
                    #exec_name,
                    arg,
                    USAGE,
                    #exec_name,
                );
            }
            fn err_no_val_allowed(arg: &str) {
                eprintln!(
                    "{}: option '{}' doesn't allow an argument\n{}\nTry '{} --help' for more information.",
                    #exec_name,
                    arg,
                    USAGE,
                    #exec_name,
                );
            }
            fn err_opt_requires_arg(arg: &str) {
                eprintln!(
                    "{}: option '{}' requires an argument\n{}\nTry '{} --help' for more information.",
                    #exec_name,
                    arg,
                    USAGE,
                    #exec_name,
                );
            }
            fn err_short_opt_requires_arg(ch: char) {
                eprintln!(
                    "{}: option requires an argument -- '{}'\n{}\nTry '{} --help' for more information.",
                    #exec_name,
                    ch,
                    USAGE,
                    #exec_name,
                );
            }
            use std::cell::RefCell;
            use std::collections::HashMap;
            const USAGE: &str = #usage;
            const HELP: &str = #help;
            pub struct Revparse {
                #rvp_pos_args_field_dec
                #dec_pub_struct_fields
            }
            struct Inner<'a> {
                #dec_inner_struct_fields
            }
            struct MutRevparse {
                #dec_mut_struct_fields
            }
            enum ArgKind {
                Short(&'static str, char),
                NoShort(&'static str),
            }
            pub struct ArgVal {
                pub value: RefCell<Option<String>>,
                kind: ArgKind,
            }
            pub struct ArgBool {
                pub value: RefCell<bool>,
                kind: ArgKind,
            }
            trait Argument {
                fn take_val(&self) -> bool;
                fn insert(&self, arg: String);
                fn was_called(&self);
                fn long(&self) -> &str;
                fn short(&self) -> char;
            }
            impl Argument for ArgVal {
                fn take_val(&self) -> bool {
                    true
                }
                fn insert(&self, arg: String) {
                    *self.value.borrow_mut() = Some(arg);
                }
                fn was_called(&self) {
                    panic!("impossible");
                }
                fn long(&self) -> &str {
                    use ArgKind::*;
                    match self.kind {
                        Short(long, _) | NoShort(long) => long,
                    }
                }
                fn short(&self) -> char {
                    use ArgKind::*;
                    match self.kind {
                        Short(_, short) => short,
                        _ => panic!("logic error"),
                    }
                }
            }
            impl Argument for ArgBool {
                fn take_val(&self) -> bool {
                    false
                }
                fn insert(&self, _: String) {
                    panic!("impossible");
                }
                fn was_called(&self) {
                    *self.value.borrow_mut() = true;
                }
                fn long(&self) -> &str {
                    use ArgKind::*;
                    match self.kind {
                        Short(long, _) | NoShort(long) => long,
                    }
                }
                fn short(&self) -> char {
                    use ArgKind::*;
                    match self.kind {
                        Short(_, short) => short,
                        _ => panic!("logic error"),
                    }
                }
            }
            pub fn print_help() {
                print!("{}{}", USAGE, HELP);
            }
            pub fn print_usage() {
                println!("{}", USAGE);
            }
            impl Revparse {
                pub fn custom_new(args: impl Iterator<Item = String>) -> Self {
                    let mut_rvp = MutRevparse {
                        #init_mut_struct_fields
                    };
                    let mut inner = Inner {
                        #init_inner_struct_fields
                    };
                    #init_hashmap
                    #parser_logic
                    Self {
                        #rvp_pos_args_field_init
                        #init_pub_struct_fields
                    }
                }
                pub fn new() -> Self {
                    Self::custom_new(std::env::args())
                }
                #get_pos_args_function
            }
            impl Inner<'_> {
                #pos_control_function
                #add_pos_internal
                fn get_str(&self, string: &str) -> &dyn Argument {
                    match self.hashmap_long.get(string) {
                        Some(arg_struct) => *arg_struct,
                        None => {
                            err_arg_does_not_exist(string);
                            exit(1);
                        },
                    }
                }
                fn get_char(&self, ch: char) -> &dyn Argument {
                    match self.hashmap_short.get(&ch) {
                        Some(arg_struct) => *arg_struct,
                        None => {
                            err_short_arg_does_not_exist(ch);
                            exit(1);
                        },
                    }
                }
            }
        }
    }
}
impl Parse for RevparseInt {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut revparse_int = RevparseInt {
            exec_name: String::from("program_name"),
            usage: String::new(),
            help: String::new(),
            min_pos_args: 0,
            custom_max_pos_args: false,
            max_pos_args: 0,
            pos_arg_help: None,
            infinite_pos_args: false,
            args: Vec::new(),
            pres_pos_args: Vec::new(),
            function_name: Ident::new("revparse", Span::call_site()),
            mod_name: Ident::new("revmod", Span::call_site()),
        };
        loop {
            if input.is_empty() {
                break;
            }
            let content;
            bracketed!(content in input);
            let parsed_content: ArgKind = content.parse()?;
            match parsed_content {
                ArgKind::Setting(arg_setting) => revparse_int.match_setting(arg_setting),
                _ => revparse_int.args.push(parsed_content),
            }
            input.parse::<Token![;]>()?;
        }
        revparse_int.create_help();
        if !revparse_int.custom_max_pos_args {
            revparse_int.max_pos_args = revparse_int.pres_pos_args.len() as u64;
        }
        Ok(revparse_int)
    }
}
#[proc_macro]
pub fn revparse(input: TokenStream) -> TokenStream {
    let revparse_int = parse_macro_input!(input as RevparseInt);
    let module = revparse_int.mk_module();
    let RevparseInt { mod_name, .. } = revparse_int;
    quote! {
        mod #mod_name {
            #module
        }
    }
    .into()
}
