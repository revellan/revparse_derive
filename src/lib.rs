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
            let i = arg.long_name();
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
                    #field_name: u8,
                }
            } else if *x <= std::u16::MAX as u64 {
                quote! {
                    #field_name: u16,
                }
            } else if *x <= std::u32::MAX as u64 {
                quote! {
                    #field_name: u32,
                }
            } else {
                quote! {
                    #field_name: u64,
                }
            }
        };
        let min_pos_args_field =
            decide_type(min_pos_args, Ident::new("min_pos_args", Span::call_site()));
        let max_pos_args_field =
            decide_type(max_pos_args, Ident::new("max_pos_args", Span::call_site()));
        let raw_add = quote! {
            self.pos_args.push(arg);
        };
        let pos_struct_field = quote! {
            pos_args: Vec<String>,
        };
        let pos_init = quote! {
            pos_args: Vec::new(),
        };
        let min_init = quote! {
            min_pos_args: #min_pos_args,
        };
        let max_init = quote! {
            max_pos_args: #max_pos_args,
        };
        let main_function;
        let declare_fields;
        let init_fields;
        let control_function;
        if *infinite_pos_args && *min_pos_args == 0 {
            main_function = quote! {
                fn add_pos_internal(&mut self, arg: String) {
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
                fn add_pos_internal(&mut self, arg: String) {
                    if self.min_pos_args != 0 {
                        self.min_pos_args -= 1;
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
                    if self.min_pos_args != 0 {
                        print_usage();
                        exit(1);
                    }
                }
            };
        } else if *min_pos_args == 0 {
            main_function = quote! {
                fn add_pos_internal(&mut self, arg: String) {
                    if self.max_pos_args == 0 {
                        err_extra_operand(&arg);
                        exit(1);
                    }
                    self.max_pos_args -= 1;
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
                fn add_pos_internal(&mut self, arg: String) {
                    if self.max_pos_args == 0 {
                        err_extra_operand(&arg);
                        exit(1);
                    }
                    self.max_pos_args -= 1;
                    if self.min_pos_args != 0 {
                        self.min_pos_args -= 1;
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
                    if self.min_pos_args != 0 {
                        print_usage();
                        exit(1);
                    }
                }
            };
        }
        (main_function, declare_fields, init_fields, control_function)
    }
    fn dec_pub_struct_fields(&self) -> TokenStream2 {
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
            Some(fields) => {
                quote! {
                    #fields
                }
            }
            None => TokenStream2::new(),
        };
        quote! {
            hashmap_long: HashMap<&'static str, RefCell<dyn Argument>>,
            hashmap_short: HashMap<char, RefCell<dyn Argument>>,
            #dec_fields
        }
    }
    fn init_inner_struct_fields(&self, init_pos_fields: Option<TokenStream2>) -> TokenStream2 {
        let init_fields = match init_pos_fields {
            Some(tokens) => quote! {
                hashmap_long: HashMap::new(),
                hashmap_short: HashMap::new(),
                #tokens
            },
            None => quote! {
                hashmap_long: HashMap::new(),
                hashmap_short: HashMap::new(),
            },
        };
        init_fields
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
                let long_str = format!("--{}", long.to_string());
                if let Some(short) = short {
                    quote! {
                        inner.hashmap_long.insert(#long_str, &mut parser.#long);
                        inner.hashmap_short.insert(#short, &mut parser.#long)
                    }
                } else {
                    quote! {
                        inner.hashmap_long.insert(#long_str, &mut parser.#long);
                    }
                }
            })
            .collect()
    }
    fn init_pub_struct_fields(&self) -> TokenStream2 {
        (&self)
            .args
            .iter()
            .map(|i| {
                use ArgKind::*;
                match i {
                    Both { long, short, .. } => {
                        let long_str = format!("--{}", long.to_string());
                        quote! {
                            #long: ArgVal { value: None, kind: ArgKind::Short(#long_str, #short)},
                        }
                    }
                    NoVal { long, short, .. } => {
                        let long_str = format!("--{}", long.to_string());
                        quote! {
                            #long: ArgBool { value: false, kind: ArgKind::Short(#long_str, #short)},
                        }
                    }
                    NoShort { long, .. } => {
                        let long_str = format!("--{}", long.to_string());
                        quote! {
                            #long: ArgVal { value: None, kind: ArgKind::NoShort(#long_str)},
                        }
                    }
                    Neither { long, .. } => {
                        let long_str = format!("--{}", long.to_string());
                        quote! {
                            #long: ArgBool { value: false, kind: ArgKind::NoShort(#long_str)},
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
        let RevparseInt { help, usage, .. } = self;
        let dec_pub_struct_fields = self.dec_pub_struct_fields();
        let init_pub_struct_fields = self.init_pub_struct_fields();
        let dec_inner_struct_fields;
        let init_inner_struct_fields;
        let pos_control_function;
        let init_hashmap = self.init_hashmap();
        let add_pos_internal;
        let parser_logic;
        let parser_boilerplate = quote! {
            // THERE IS SOME NON BOILERPLATE CODE IN HERE, GET IT OUT!!!
            else if e_arg.starts_with("--") {
                match e_arg
                    .split_once('=')
                    .map(|(arg_name, val)| (arg_name, val.to_string()))
                {
                    Some((arg_name, val)) => {
                        let refcell = inner.get_str(arg_name);
                        if refcell.borrow().take_val() {
                            refcell.borrow_mut().insert(val);
                        } else {
                            inner.err_no_val_allowed(&arg_name);
                            exit(1);
                        }
                    },
                    None => {
                        let refcell = inner.get_str(e_arg);
                        if refcell.borrow().take_val() {
                            next_is_val = Some(refcell);
                        } else {
                            refcell.borrow_mut().was_called();
                        }
                    },
                }
            } else if e_arg.starts_with('-') {
                let mut rest_is_val: Option<Refcell<dyn Argument>> = None;
                let mut value = String::new();
                'chars: for e_char in e_arg.chars().skip(1) {
                    if rest_is_val.is_some() {
                        value.push(e_char);
                    } else {
                        let refcell = inner.get_char(e_char);
                        if refcell.borrow().take_val() {
                            rest_is_val = Some(refcell);
                        } else {
                            refcell.borrow_mut().was_called();
                        }
                    }
                }
                if let Some(refcell) = rest_is_val.take() {
                    if value.len() == 0 {
                        next_is_val = Some(refcell, false);
                    } else {
                        refcell.borrow_mut().insert(value);
                    }
                }
            } 
        };
        if positional_logic {
            let (main_function, declare_fields, init_fields, control_function) =
                self.mk_add_pos_internal();
            dec_inner_struct_fields = self.dec_inner_struct_fields(Some(declare_fields));
            init_inner_struct_fields = self.init_inner_struct_fields(Some(init_fields));
            pos_control_function = control_function;
            add_pos_internal = main_function;
            parser_logic = quote! {
                let mut next_is_val: Option<(RefCell<dyn Argument>, bool)> = None;
                let mut next_is_pos = false;
                'outer for e_arg in args.skip(1) {
                    if let Some((refcell, _)) = next_is_val.take() {
                        refcell.borrow_mut().insert(e_arg);
                    } else if next_is_pos {
                        inner.add_pos_internal(e_arg);
                    } else if e_arg == "--help" || e_arg == "-h" {
                        print_help();
                        exit(0);
                    } else if e_arg == "--" {
                        next_is_pos = true;
                    }
                    #parser_boilerplate
                    else {
                        inner.add_pos_internal(e_arg);
                    }
                }
                if let Some(refcell, is_long) = next_is_val.take() {
                    if is_long {
                        err_opt_requires_arg(refcell.borrow().long());
                        exit(1);
                    } else {
                        err_short_opt_requires_arg(refcell.borrow().short());
                        exit(1);
                    }
                }
                inner.check_pos();
            };
        } else {
            dec_inner_struct_fields = self.dec_inner_struct_fields(None);
            init_inner_struct_fields = self.init_inner_struct_fields(None);
            pos_control_function = TokenStream2::new();
            add_pos_internal = TokenStream2::new();
            parser_logic = quote! {
                let mut next_is_val: Option<(RefCell<dyn Argument>, bool)> = None;
                'outer for e_arg in args.skip(1) {
                    if let Some(refcell) = next_is_val.take() {
                        refcell.borrow_mut().insert(e_arg);
                    } else if e_arg == "--help" || e_arg == "-h" {
                        print_help();
                        exit(0);
                    } else if e_arg == "--" {
                        next_is_pos = true;
                    }
                    #parser_boilerplate
                    else {
                        inner.add_pos_internal(e_arg);
                    }
                }
                if let Some(refcell, is_long) = next_is_val.take() {
                    if is_long {
                        err_opt_requires_arg(refcell.borrow().long());
                        exit(1);
                    } else {
                        err_short_opt_requires_arg(refcell.borrow().short());
                        exit(1);
                    }
                }
            };
        }
        let whole_mod = quote! {
            use std::cell::RefCell;
            use std::collections::HashMap;
            const USAGE: &str = #usage;
            const HELP: &str = #help;
            pub struct Rvp {
                #dec_pub_struct_fields
            }
            struct Inner {
                #dec_inner_struct_fields
            }
            enum ArgKind {
                Short(&'static str, char),
                NoShort(&'static str),
            }
            pub struct ArgVal {
                pub value: Option<String>,
                kind: ArgKind,
            }
            pub struct ArgBool {
                pub value: bool,
                kind: ArgKind,
            }
            trait Argument {
                fn take_val(&self) -> bool;
                fn insert(&mut self, arg: String);
                fn was_called(&mut self);
                fn long(&self) -> &str;
                fn short(&self) -> char;
            }
            impl Argument for ArgVal {
                fn take_val(&self) -> bool {
                    true
                }
                fn insert(&mut self, arg: String) {
                    self.value = Some(arg);
                }
                fn was_called(&mut self) {
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
                fn insert(&mut self, arg: String) {
                    panic!("impossible");
                }
                fn was_called(&mut self) {
                    self.value = true;
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
            impl Rvp {
                fn new(args: impl Iterator<Item = String>) -> Self {
                    let mut parser = Self {
                        #init_pub_struct_fields
                    };
                    let mut inner = Inner {
                        #init_inner_struct_fields
                    };
                    #init_hashmap
                    #parser_logic
                }
            }
            impl Inner {
                #pos_control_function
                #add_pos_internal
                fn get_str(&self, string: &str) -> RefCell<dyn Argument> {
                    match self.hashmap_long.get(string) {
                        Some(ref_cell) => ref_cell,
                        None => {
                            self.err_arg_does_not_exist(string);
                            exit(1);
                        },
                    }
                }
                fn get_char(&self, ch: char) -> RefCell<dyn Arguments> {
                    match self.hashmap_short.get(ch) {
                        Some(ref_cell) => ref_cell,
                        None => {
                            self.err_short_arg_does_not_exist(ch);
                            exit(1);
                        },
                    }
                }
            }
        };
        whole_mod
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
    let RevparseInt {
        usage,
        help,
        min_pos_args,
        max_pos_args,
        pos_arg_help,
        infinite_pos_args,
        args,
        pres_pos_args,
        function_name,
        mod_name,
        ..
    } = revparse_int;
    let struct_fields: TokenStream2 = args
        .iter()
        .map(|x| {
            use ArgKind::*;
            match x {
                Both { long, .. } | NoShort { long, .. } => {
                    quote! {
                        #long: Option<String>,
                    }
                }
                NoVal { long, .. } | Neither { long, .. } => {
                    quote! {
                        #long: bool,
                    }
                }
                _ => panic!("{}", IMPOSSIBLE_ERROR),
            }
        })
        .collect();
    let module = TokenStream2::new();
    let after_module = quote! {
        use #mod_name::Rvp;
    };
    let final_tokenstream = quote! {
        mod #mod_name {
            #module
        }
        #after_module
    };
    final_tokenstream.into()
}
