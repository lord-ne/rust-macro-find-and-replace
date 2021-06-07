//! # macro_find_and_replace
//!
//! This crate provides macros to find-and-replace tokens. 
//! 
//! For more information about tokens in general and what counts as a single token, see the [TokenStream](https://doc.rust-lang.org/proc_macro/struct.TokenStream.html) type in the `proc_macro` crate, which is the input and output type of procedural macros.

extern crate proc_macro;
use proc_macro::{TokenStream, TokenTree, Group};

/// Replaces all occurences of a given single token with another single token.
/// 
/// Takes arguments in the order `replace_token!{needle, replacement, code to search}`. You may use any kind of brackets.
/// 
/// # Panics
/// At compile time, if the arguments cannot be correctly parsed.
/// 
/// # Examples
///
/// Replaces all plus signs with minus signs:
/// ```
/// # extern crate macro_find_and_replace;
/// # use macro_find_and_replace::replace_token;
/// replace_token!{+, -,
///     let x = (1 + 2) + 3;
/// }
/// assert_eq!(x, -4); 
/// ```
/// 
/// Replaces the identifier `MY_ARRAY` with an array literal
/// ```
/// # extern crate macro_find_and_replace;
/// # use macro_find_and_replace::replace_token;
/// replace_token!{MY_ARRAY, [8, 0],
///     let x = MY_ARRAY;
/// }
/// assert_eq!(x, [8, 0]);
/// ```
#[proc_macro]
pub fn replace_token(raw_input: TokenStream) -> TokenStream {
    let (needle, replacement, input) = parse_args(raw_input);
    return TokenReplacer{needle, replacement}.process(input);
}

struct TokenReplacer {
    needle: TokenTree,
    replacement: TokenTree
}

impl TokenReplacer {
    fn process(&self, input: TokenStream) -> TokenStream {
        input.into_iter().map(|t| {
            match t {
                _ if token_eq(&t, &self.needle) => self.replacement.clone(),
                TokenTree::Group(g) => TokenTree::Group(Group::new(g.delimiter(), self.process(g.stream()))),
                _ => t
            } 
        }).collect()
    } 
}

/// Replaces all occurences of a given sequence of tokens with another sequence of tokens.
/// 
/// Takes arguments in the form `replace_token!{[needle tokens], [replacement tokens], code to search}`. 
/// The brackets around the needle and replacement tokens are not included in the search.
/// You may use any kind of brackets.
/// 
/// # Panics
/// At compile time, if the arguments cannot be correctly parsed.
/// 
/// # Examples
/// 
/// Replaces the sequence `9 + 10` with `21`. Note that spacing between tokens is generally ignored:
/// ```
/// # extern crate macro_find_and_replace;
/// # use macro_find_and_replace::replace_token_sequence;
/// replace_token_sequence!{[9 + 10], [21],
///     let x = 9+10;
/// }
/// assert_eq!(x, 21);
/// ```
/// 
/// Replaces the content of a match arm:
/// ```
/// # extern crate macro_find_and_replace;
/// # use macro_find_and_replace::replace_token_sequence;
/// enum Numbers {
///     I32(i32),
///     I64(i64),
///     ISize(isize)
/// }
/// let n = Numbers::I32(12);
/// replace_token_sequence!{[MY_ARM], [(y * 2) as f64],
///     let x = match(n) {
///         Numbers::I32(y) => MY_ARM,
///         Numbers::I64(y) => MY_ARM,
///         Numbers::ISize(y) => MY_ARM,
///     };
/// }
/// assert_eq!(x, 24.0);
/// ```
#[proc_macro]
pub fn replace_token_sequence(raw_input: TokenStream) -> TokenStream {
    // Destructure or panic
    let (needle_tree, replacement_tree, input) = if let (TokenTree::Group(n), TokenTree::Group(r), i) = parse_args(raw_input) {
        (n.stream(), r.stream(), i)
    } else {
        panic!("{}", "First or second argument to replace_token_sequence!() was not a group.
The correct usage is: replace_token_sequence!{[needle], [replacement], code to search}");
    };

    let mut needle : Vec<TokenTree> = needle_tree.into_iter().collect();
    let mut replacement : Vec<TokenTree> = replacement_tree.into_iter().collect();

    // Default to single replacement if needle and replacement are both singleton
    if needle.len() == 1 && replacement.len() == 1 {
        return TokenReplacer{needle : needle.swap_remove(0), replacement : replacement.swap_remove(0)}.process(input);
    }
    
    return TokenSequenceReplacer{needle, replacement}.process(input);
}

struct TokenSequenceReplacer {
    needle: Vec<TokenTree>,
    replacement: Vec<TokenTree>
}

impl TokenSequenceReplacer {
    fn process(&self, input: TokenStream) -> TokenStream {
        let mut buffer : Vec<TokenTree> =  Vec::with_capacity(self.needle.len());
        let mut output : Vec<TokenTree> =  Vec::new();

        for mut t in input {
            if let TokenTree::Group(g) = t {
                t = TokenTree::Group(Group::new(g.delimiter(), self.process(g.stream())));
            }
            
            if buffer.len() < self.needle.len() - 1 {
                buffer.push(t);
                continue;
            } else if buffer.len() == self.needle.len() - 1 {
                buffer.push(t);
            }
            else {
                output.push(buffer.remove(0));
                buffer.push(t); 
            }
            
            if iters_eq_by(buffer.iter(), self.needle.iter(), |a, b| {token_eq(a, b)}) {
                buffer.clear();
                output.append(&mut self.replacement.clone())
            }
        }
        return output.into_iter().chain(buffer).collect();
    }
}

fn iters_eq_by<I, J, F>(a: I, b: J, mut eq: F) -> bool 
where 
    I: IntoIterator,
    J: IntoIterator,
    F: FnMut(I::Item, J::Item) -> bool
{
    let mut a = a.into_iter();
    let mut b = b.into_iter();
   
    loop {
        match (a.next(), b.next()) {
            (Some(a), Some(b)) => if !eq(a, b) { return false; },
            (None, None) => break,
            (Some(_), None) | (None, Some(_)) => return false,
        }
    } 
    return true;
}

fn token_eq(a: &TokenTree, b: &TokenTree) -> bool {
    match (a, b) {
        (TokenTree::Ident(a), TokenTree::Ident(b)) =>  a.to_string() == b.to_string(),
        // Checking for a.spacing() == b.spacing() as well is too conservative
        (TokenTree::Punct(a), TokenTree::Punct(b)) => a.to_string() == b.to_string(),
        (TokenTree::Literal(a), TokenTree::Literal(b)) => a.to_string() == b.to_string(),
        (TokenTree::Group(a), TokenTree::Group(b)) => {
            (a.delimiter() == b.delimiter()) 
            && (a.to_string() == b.to_string()) 
            && iters_eq_by(a.clone().stream(), b.clone().stream(), |a, b| {token_eq(&a, &b)})
        },
        (_, _) => false,
    }
}

fn parse_args(input: TokenStream) -> (TokenTree, TokenTree, TokenStream) {
    let mut iter = input.into_iter();  

    let needle = iter.next().expect("Not enough tokens passed to macro");

    let comma = iter.next().expect("Not enough tokens passed to macro"); // Ignore comma
    assert_eq!(comma.to_string(), ",", "Malformed arguments to macro: second token was not a comma");

    let replacement = iter.next().expect("Not enough tokens passed to macro");

    let comma = iter.next().expect("Not enough tokens passed to macro"); // Ignore comma
    assert_eq!(comma.to_string(), ",", "Malformed arguments to macro: fourth token was not a comma"); 

    return (needle, replacement, iter.collect());
}