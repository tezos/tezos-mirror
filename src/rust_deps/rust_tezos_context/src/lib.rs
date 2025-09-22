use std::{ops::DerefMut, rc::Rc};

use ocaml::{FromValue, Pointer, Runtime, Value};
use tezos_context::{
    self, IndexApi, ProtocolContextApi, ShellContextApi, TezedgeContext, TezedgeIndex,
    initializer::initialize_tezedge_index,
    working_tree::{
        DirEntryKind,
        storage::DirectoryId,
        working_tree::{FoldDepth, FoldOrder, TreeWalker, WorkingTree},
    },
};
use tezos_context_api::{
    ContextKvStoreConfiguration, TezosContextTezEdgeStorageConfiguration,
    TezosContextTezedgeOnDiskBackendOptions,
};
use tezos_crypto_rs::hash::{ContextHash, HashTrait};

#[ocaml::sig]
pub struct Index(TezedgeIndex);
ocaml::custom!(Index);

#[ocaml::sig]
pub struct Context(TezedgeContext);
ocaml::custom!(Context);

#[ocaml::sig]
pub enum Tree {
    Owned(WorkingTree),
    Shared(Rc<WorkingTree>),
}
ocaml::custom!(Tree);

impl From<WorkingTree> for Tree {
    fn from(t: WorkingTree) -> Self {
        Self::Owned(t)
    }
}

impl From<Rc<WorkingTree>> for Tree {
    fn from(t: Rc<WorkingTree>) -> Self {
        Self::Shared(t)
    }
}

impl Tree {
    pub fn as_ref(self: &Self) -> &WorkingTree {
        match self {
            Self::Owned(tree) => tree,
            Self::Shared(tree) => tree,
        }
    }
}

fn value_of_context_hash(h: ContextHash) -> Value {
    unsafe { ocaml::Value::bytes(h.as_ref()) }
}

fn context_hash_of_value(origin: &str, v: Value) -> ContextHash {
    let context_hash = unsafe { v.field(0) };
    let context_hash = unsafe { context_hash.bytes_val() };
    ContextHash::try_from_bytes(context_hash).expect(origin)
}

#[ocaml::func]
#[ocaml::sig("string -> index")]
pub fn index_init(base_path: String) -> Pointer<Index> {
    let storage_configuration = TezosContextTezEdgeStorageConfiguration {
        backend: ContextKvStoreConfiguration::OnDisk(TezosContextTezedgeOnDiskBackendOptions {
            base_path,
            startup_check: true,
        }),
        ipc_socket_path: None,
    };
    Pointer::alloc_custom(Index(
        initialize_tezedge_index(&storage_configuration, None).expect("index_init"),
    ))
}

#[ocaml::func]
#[ocaml::sig("index -> unit")]
pub fn index_close(index: Pointer<Index>) {
    let index = index.as_ref();
    index.0.close();
}

#[ocaml::func]
#[ocaml::sig("context -> index")]
pub fn index(mut context: Pointer<Context>) -> Pointer<Index> {
    let context = context.as_ref();
    Pointer::alloc_custom(Index(context.0.index.clone()))
}

#[ocaml::func]
#[ocaml::sig("index -> context")]
pub fn context_init(mut index: Pointer<Index>) -> Pointer<Context> {
    let index = index.as_mut();
    let context = TezedgeContext::new(index.0.clone(), None, None);
    Pointer::alloc_custom(Context(context))
}

#[ocaml::func]
#[ocaml::sig("context -> int64 -> string -> string -> bytes")]
pub fn commit(mut context: Pointer<Context>, time: i64, author: String, message: String) -> Value {
    let context = context.as_mut();
    let hash = context
        .0
        .commit(author.into(), message.into(), time)
        .expect("commit");
    value_of_context_hash(hash)
}

#[ocaml::func]
#[ocaml::sig("context -> int64 -> string -> string -> bytes")]
pub fn hash(mut context: Pointer<Context>, time: i64, author: String, message: String) -> Value {
    let context = context.as_mut();
    let hash = context.0.hash(author, message, time).expect("hash");
    value_of_context_hash(hash)
}

#[ocaml::func]
#[ocaml::sig("index -> 'a -> bool")]
pub fn exists(mut index: Pointer<Index>, context_hash: Value) -> bool {
    let context_hash = context_hash_of_value("exists", context_hash);
    let index = index.as_mut();
    index.0.exists(&context_hash).expect("exists")
}

#[ocaml::func]
#[ocaml::sig("index -> 'a -> context option")]
pub fn checkout(mut index: Pointer<Index>, context_hash: Value) -> Option<Pointer<Context>> {
    let context_hash = context_hash_of_value("checkout", context_hash);
    let index = index.as_mut();
    index
        .0
        .clone()
        .checkout(&context_hash)
        .expect("checkout")
        .map(|context| Pointer::alloc_custom(Context(context)))
}

pub struct Test(Option<Vec<u8>>);

unsafe impl ocaml::ToValue for Test {
    fn to_value(&self, rt: &Runtime) -> Value {
        unsafe {
            match &self.0 {
                Some(vec) => {
                    let bytes = ocaml::Value::bytes(vec);
                    ocaml::Value::some(rt, bytes)
                }
                None => Value::none(),
            }
        }
    }
}

#[ocaml::func]
#[ocaml::sig("context -> string list -> bytes option")]
pub fn find(mut ctxt: Pointer<Context>, key: ocaml::List<String>) -> Test {
    let ctxt = ctxt.as_mut();
    let key: Vec<String> = key.into_vec();
    let key: Vec<&str> = key.iter().map(|s| s.as_str()).collect();
    let r = ctxt.0.clone().find(&key);
    Test(r.expect("find"))
}

#[ocaml::func]
#[ocaml::sig("context -> string list -> context")]
pub fn remove(mut ctxt: Pointer<Context>, key: ocaml::List<String>) -> Pointer<Context> {
    let x = ctxt.as_mut();
    let key: Vec<String> = key.into_vec();
    let key: Vec<&str> = key.iter().map(|s| s.as_str()).collect();
    Pointer::alloc_custom(Context(x.0.delete(&key).expect("remove")))
}

#[ocaml::func]
#[ocaml::sig("context -> string list -> 'a -> context")]
pub fn add(mut ctxt: Pointer<Context>, key: ocaml::List<String>, value: Value) -> Pointer<Context> {
    let ctxt = ctxt.as_mut();
    let key: Vec<String> = key.into_vec();
    let key: Vec<&str> = key.iter().map(|s| s.as_str()).collect();
    let value = unsafe { value.bytes_val() };
    Pointer::alloc_custom(Context(ctxt.0.clone().add(&key, value).expect("add")))
}

#[ocaml::func]
#[ocaml::sig("context -> string list -> bool")]
pub fn mem_tree(mut ctxt: Pointer<Context>, key: ocaml::List<String>) -> bool {
    let x = ctxt.as_mut();
    let key: Vec<String> = key.into_vec();
    let key: Vec<&str> = key.iter().map(|s| s.as_str()).collect();
    x.0.mem_tree(&key)
}

#[ocaml::func]
#[ocaml::sig("context -> string list -> bool")]
pub fn mem(mut ctxt: Pointer<Context>, key: ocaml::List<String>) -> bool {
    let x = ctxt.as_mut();
    let key: Vec<String> = key.into_vec();
    let key: Vec<&str> = key.iter().map(|s| s.as_str()).collect();
    x.0.mem(&key).expect("mem")
}

#[ocaml::func]
#[ocaml::sig("context -> string list -> tree option")]
pub fn find_tree(mut ctxt: Pointer<Context>, key: ocaml::List<String>) -> Option<Pointer<Tree>> {
    let x = ctxt.as_mut();
    let key: Vec<String> = key.into_vec();
    let key: Vec<&str> = key.iter().map(|s| s.as_str()).collect();
    x.0.find_tree(&key)
        .expect("find_tree")
        .map(|tree| Pointer::alloc_custom(tree.into()))
}

#[ocaml::func]
#[ocaml::sig("context -> int option -> int option -> string list -> (string * tree) array")]
pub fn list(
    mut ctxt: Pointer<Context>,
    offset: Option<usize>,
    length: Option<usize>,
    key: ocaml::List<String>,
) -> Vec<(String, Pointer<Tree>)> {
    let x = ctxt.as_mut();
    let key: Vec<String> = key.into_vec();
    let key: Vec<&str> = key.iter().map(|s| s.as_str()).collect();
    x.0.list(offset, length, &key)
        .expect("list")
        .into_iter()
        .map(|(k, tree)| (k, Pointer::alloc_custom(tree.into())))
        .collect()
}

#[ocaml::func]
#[ocaml::sig("context -> string list -> int")]
pub fn length(mut ctxt: Pointer<Context>, key: ocaml::List<String>) -> usize {
    let x = ctxt.as_mut();
    let key: Vec<String> = key.into_vec();
    let key: Vec<&str> = key.iter().map(|s| s.as_str()).collect();
    // TODO: There is a dedicated function in tezedge.
    x.0.list(None, None, &key).expect("length").len()
}

#[ocaml::func]
#[ocaml::sig("context -> string list -> tree -> context")]
pub fn add_tree(
    mut ctxt: Pointer<Context>,
    key: ocaml::List<String>,
    mut tree: Pointer<Tree>,
) -> Pointer<Context> {
    let x = ctxt.as_mut();
    let key: Vec<String> = key.into_vec();
    let key: Vec<&str> = key.iter().map(|s| s.as_str()).collect();
    Pointer::alloc_custom(Context(
        x.0.add_tree(&key, tree.as_ref().as_ref())
            .expect("add_tree"),
    ))
}

#[ocaml::sig]
pub struct OCamlTreeWalker(TreeWalker);
ocaml::custom!(OCamlTreeWalker);

#[derive(ocaml::FromValue)]
#[ocaml::sig("Eq of int | Le of int | Lt of int | Ge of int | Gt of int")]
pub enum OCamlDepth {
    Eq(usize),
    Le(usize),
    Lt(usize),
    Ge(usize),
    Gt(usize),
}

impl From<OCamlDepth> for FoldDepth {
    fn from(v: OCamlDepth) -> Self {
        match v {
            OCamlDepth::Eq(i) => Self::Eq(i as i64),
            OCamlDepth::Le(i) => Self::Le(i as i64),
            OCamlDepth::Lt(i) => Self::Lt(i as i64),
            OCamlDepth::Ge(i) => Self::Ge(i as i64),
            OCamlDepth::Gt(i) => Self::Gt(i as i64),
        }
    }
}

#[derive(ocaml::FromValue)]
#[ocaml::sig("Sorted | Undefined")]
pub enum OCamlOrder {
    Sorted,
    Undefined,
}

impl From<OCamlOrder> for FoldOrder {
    fn from(v: OCamlOrder) -> Self {
        match v {
            OCamlOrder::Sorted => Self::Sorted,
            OCamlOrder::Undefined => Self::Undefined,
        }
    }
}
#[ocaml::func]
#[ocaml::sig("tree -> o_caml_depth option -> string list -> 'b -> o_caml_tree_walker")]
pub fn make_tree_walker(
    tree: Pointer<Tree>,
    depth: Option<OCamlDepth>,
    key: ocaml::List<String>,
    order: OCamlOrder,
) -> Pointer<OCamlTreeWalker> {
    let tree = tree.as_ref();
    let key: Vec<String> = key.into_vec();
    let key: Vec<&str> = key.iter().map(|s| s.as_str()).collect();
    let walker = tree
        .as_ref()
        .fold_iter(depth.map(OCamlDepth::into), &key, order.into())
        .expect("make_tree_walker");
    Pointer::alloc_custom(OCamlTreeWalker(walker))
}

#[ocaml::func]
#[ocaml::sig("o_caml_tree_walker -> (string array * tree) option")]
pub fn tree_walker_next(
    mut tree_walker: Pointer<OCamlTreeWalker>,
) -> Option<(Vec<String>, Pointer<Tree>)> {
    let tree_walker = tree_walker.as_mut();
    tree_walker
        .0
        .next()
        .map(|(keys, tree)| (keys, Pointer::alloc_custom(tree.into())))
}

#[ocaml::func]
#[ocaml::sig("context -> tree")]
pub fn get_tree(mut context: Pointer<Context>) -> Pointer<Tree> {
    let context: TezedgeContext = context.as_mut().0.clone();
    let tree = Rc::clone(&context.tree);
    ocaml::Pointer::alloc_custom(tree.into())
}

#[derive(ocaml::ToValue)]
#[ocaml::sig("Value | Tree")]
pub enum Kind {
    Value,
    Tree,
}

impl From<DirEntryKind> for Kind {
    fn from(k: DirEntryKind) -> Self {
        match k {
            DirEntryKind::Blob => Self::Value,
            DirEntryKind::Directory => Self::Tree,
        }
    }
}

#[ocaml::func]
#[ocaml::sig("tree -> kind")]
pub fn tree_kind(tree: Pointer<Tree>) -> Kind {
    tree.as_ref().as_ref().kind().into()
}

#[ocaml::func]
#[ocaml::sig("tree -> string list -> bool")]
pub fn tree_mem(tree: Pointer<Tree>, key: ocaml::List<String>) -> bool {
    let key: Vec<String> = key.into_vec();
    let key: Vec<&str> = key.iter().map(|s| s.as_str()).collect();
    tree.as_ref().as_ref().mem(&key).expect("tree_mem")
}

#[ocaml::func]
#[ocaml::sig("tree -> string list  -> bool")]
pub fn tree_mem_tree(tree: Pointer<Tree>, key: ocaml::List<String>) -> bool {
    let key: Vec<String> = key.into_vec();
    let key: Vec<&str> = key.iter().map(|s| s.as_str()).collect();
    tree.as_ref().as_ref().mem_tree(&key)
}

#[ocaml::func]
#[ocaml::sig("tree -> string list -> bytes option")]
pub fn tree_find(tree: Pointer<Tree>, key: ocaml::List<String>) -> Test {
    let key: Vec<String> = key.into_vec();
    let key: Vec<&str> = key.iter().map(|s| s.as_str()).collect();
    Test(tree.as_ref().as_ref().find(&key).expect("tree_find"))
}

#[ocaml::func]
#[ocaml::sig("tree -> string list -> tree option")]
pub fn tree_find_tree(tree: Pointer<Tree>, key: ocaml::List<String>) -> Option<Pointer<Tree>> {
    let key: Vec<String> = key.into_vec();
    let key: Vec<&str> = key.iter().map(|s| s.as_str()).collect();
    tree.as_ref()
        .as_ref()
        .find_tree(&key)
        .expect("tree_find_tree")
        .map(|tree| Pointer::alloc_custom(tree.into()))
}

#[ocaml::func]
#[ocaml::sig("tree -> int option -> int option -> string list -> (string * tree) array")]
pub fn tree_list(
    tree: Pointer<Tree>,
    offset: Option<usize>,
    length: Option<usize>,
    key: ocaml::List<String>,
) -> Vec<(String, Pointer<Tree>)> {
    let key: Vec<String> = key.into_vec();
    let key: Vec<&str> = key.iter().map(|s| s.as_str()).collect();
    tree.as_ref()
        .as_ref()
        .list(offset, length, &key)
        .unwrap()
        .into_iter()
        .map(|(k, tree)| (k, Pointer::alloc_custom(tree.into())))
        .collect()
}

#[ocaml::func]
#[ocaml::sig("tree -> string list -> int")]
pub fn tree_length(tree: Pointer<Tree>, key: ocaml::List<String>) -> usize {
    let key: Vec<String> = key.into_vec();
    let key: Vec<&str> = key.iter().map(|s| s.as_str()).collect();
    tree.as_ref().as_ref().list(None, None, &key).unwrap().len()
}
#[ocaml::func]
#[ocaml::sig("tree -> string list -> 'a -> tree")]
pub fn tree_add(tree: Pointer<Tree>, key: ocaml::List<String>, value: Value) -> Pointer<Tree> {
    let key: Vec<String> = key.into_vec();
    let key: Vec<&str> = key.iter().map(|s| s.as_str()).collect();
    let value = unsafe { value.bytes_val() };
    let new_tree = tree.as_ref().as_ref().add(&key, value).unwrap();
    Pointer::alloc_custom(new_tree.into())
}

#[ocaml::func]
#[ocaml::sig("tree -> string list -> tree -> tree")]
pub fn tree_add_tree(
    tree: Pointer<Tree>,
    key: ocaml::List<String>,
    tree_to_add: Pointer<Tree>,
) -> Pointer<Tree> {
    let key: Vec<String> = key.into_vec();
    let key: Vec<&str> = key.iter().map(|s| s.as_str()).collect();
    let new_tree = tree
        .as_ref()
        .as_ref()
        .add_tree(&key, tree_to_add.as_ref().as_ref())
        .unwrap();
    Pointer::alloc_custom(new_tree.into())
}

#[ocaml::func]
#[ocaml::sig("tree -> string list -> tree")]
pub fn tree_remove(tree: Pointer<Tree>, key: ocaml::List<String>) -> Pointer<Tree> {
    let key: Vec<String> = key.into_vec();
    let key: Vec<&str> = key.iter().map(|s| s.as_str()).collect();
    let new_tree = tree.as_ref().as_ref().delete(&key).unwrap();
    Pointer::alloc_custom(new_tree.into())
}

#[ocaml::func]
#[ocaml::sig("tree -> bool")]
pub fn tree_is_empty(tree: Pointer<Tree>) -> bool {
    tree.as_ref().as_ref().is_empty()
}

#[ocaml::func]
#[ocaml::sig("context -> tree")]
pub fn tree_empty(mut context: Pointer<Context>) -> Pointer<Tree> {
    let context = context.as_mut();
    let tree = WorkingTree::new(context.0.clone().index);
    Pointer::alloc_custom(tree.into())
}

#[ocaml::func]
#[ocaml::sig("tree -> bytes option")]
pub fn tree_to_value(tree: Pointer<Tree>) -> Test {
    let value = tree.as_ref().as_ref().get_value();
    Test(value)
}

#[ocaml::func]
#[ocaml::sig("context -> bytes -> tree")]
pub fn tree_of_value(mut context: Pointer<Context>, value: Value) -> Pointer<Tree> {
    let context = context.as_mut().0.clone();
    let mut storage = context.index.storage.borrow_mut();
    let blob_id = unsafe { storage.add_blob_by_ref(value.bytes_val()).unwrap() };
    std::mem::drop(storage);
    let tree = WorkingTree::new_with_value(context.index, blob_id);
    Pointer::alloc_custom(tree.into())
}

#[ocaml::func]
#[ocaml::sig("tree -> bytes")]
pub fn tree_hash(tree: Pointer<Tree>) -> Value {
    let hash = tree
        .as_ref()
        .as_ref()
        .hash()
        .map(|h| (ContextHash::try_from(&h[..]).unwrap()))
        .unwrap();
    value_of_context_hash(hash)
}

#[ocaml::func]
#[ocaml::sig("tree -> tree -> bool")]
pub fn tree_equal(tree1: Pointer<Tree>, tree2: Pointer<Tree>) -> bool {
    tree1
        .as_ref()
        .as_ref()
        .equal(tree2.as_ref().as_ref())
        .unwrap()
}

#[ocaml::func]
#[ocaml::sig("string -> 'a -> string -> unit")]
pub fn export_snapshot(context_path: String, context_hash: Value, output: String) -> () {
    let context_hash = context_hash_of_value("export_snapshot", context_hash);
    tezos_context::snapshot::export_snapshot(context_path, &context_hash, output)
}
