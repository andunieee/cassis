use cassis_core::{L2Tag, NetworkId, NodeAnnouncement};
use serde::{Deserialize, Serialize};
use std::cmp::Ordering;
use std::collections::{BinaryHeap, HashMap};

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct NodeGraph {
    pub nodes: Vec<NodeAnnouncement>,
    #[serde(skip)]
    network_index: HashMap<NetworkId, Vec<usize>>,
}

impl NodeGraph {
    pub fn new(nodes: Vec<NodeAnnouncement>) -> Self {
        let mut network_index: HashMap<NetworkId, Vec<usize>> = HashMap::new();
        for (idx, node) in nodes.iter().enumerate() {
            for network in &node.networks {
                network_index
                    .entry(network.clone())
                    .or_default()
                    .push(idx);
            }
        }
        Self {
            nodes,
            network_index,
        }
    }

    fn nodes_for_network(&self, network: &NetworkId) -> impl Iterator<Item = usize> + '_ {
        self.network_index
            .get(network)
            .map(|nodes| nodes.iter().copied())
            .into_iter()
            .flatten()
    }
}

#[derive(thiserror::Error, Debug)]
pub enum NostrError {
    #[error("network error: {0}")]
    Network(String),
    #[error("invalid event: {0}")]
    InvalidEvent(String),
    #[error("unimplemented")]
    Unimplemented,
}

#[derive(thiserror::Error, Debug)]
pub enum RouteError {
    #[error("no route found")]
    NoRoute,
    #[error("invalid graph: {0}")]
    InvalidGraph(String),
    #[error("unimplemented")]
    Unimplemented,
}

#[derive(Clone, Debug)]
pub struct NostrAnnouncer {
    pub relays: Vec<String>,
}

impl NostrAnnouncer {
    pub fn new(relays: Vec<String>) -> Self {
        Self { relays }
    }

    pub async fn publish_once(
        &self,
        _announcement: &NodeAnnouncement,
    ) -> Result<(), NostrError> {
        Err(NostrError::Unimplemented)
    }

    pub async fn run_republish_loop(
        &self,
        _announcement: NodeAnnouncement,
        _interval_secs: u64,
    ) -> Result<(), NostrError> {
        Err(NostrError::Unimplemented)
    }
}

pub async fn fetch_announcements(_relays: &[String]) -> Result<Vec<NodeAnnouncement>, NostrError> {
    Err(NostrError::Unimplemented)
}

pub fn build_graph(announcements: Vec<NodeAnnouncement>) -> NodeGraph {
    NodeGraph::new(announcements)
}

pub fn find_route(
    _graph: &NodeGraph,
    _destination: &str,
    _amount_msat: u64,
    _sender_network: &NetworkId,
) -> Result<Vec<(NodeAnnouncement, L2Tag, L2Tag)>, RouteError> {
    let graph = _graph;
    let destination = _destination;
    let amount_msat = _amount_msat;
    let sender_network = _sender_network;

    let mut heap: BinaryHeap<State> = BinaryHeap::new();
    let mut dist: HashMap<StateKey, u64> = HashMap::new();
    let mut prev: HashMap<StateKey, (StateKey, NetworkId)> = HashMap::new();

    for node_idx in graph.nodes_for_network(sender_network) {
        let node = &graph.nodes[node_idx];
        let fee = node_fee_msat(node, amount_msat);
        let key = StateKey {
            node_idx,
            incoming: sender_network.clone(),
        };
        dist.insert(key.clone(), fee);
        heap.push(State { cost: fee, key });
    }

    let mut goal: Option<StateKey> = None;

    while let Some(State { cost, key }) = heap.pop() {
        if let Some(best) = dist.get(&key) {
            if cost > *best {
                continue;
            }
        }

        let node = &graph.nodes[key.node_idx];
        if node.node_pubkey == destination {
            goal = Some(key.clone());
            break;
        }

        for outgoing in &node.networks {
            for next_idx in graph.nodes_for_network(outgoing) {
                if next_idx == key.node_idx {
                    continue;
                }
                let next_key = StateKey {
                    node_idx: next_idx,
                    incoming: outgoing.clone(),
                };
                let next_node = &graph.nodes[next_idx];
                let next_cost = cost.saturating_add(node_fee_msat(next_node, amount_msat));
                let is_better = match dist.get(&next_key) {
                    Some(existing) => next_cost < *existing,
                    None => true,
                };
                if is_better {
                    dist.insert(next_key.clone(), next_cost);
                    prev.insert(next_key.clone(), (key.clone(), outgoing.clone()));
                    heap.push(State {
                        cost: next_cost,
                        key: next_key,
                    });
                }
            }
        }
    }

    let goal = goal.ok_or(RouteError::NoRoute)?;
    let mut hops_rev: Vec<(NodeAnnouncement, L2Tag, L2Tag)> = Vec::new();
    let mut current = goal.clone();
    let mut outgoing_for_current: Option<NetworkId> = None;

    loop {
        let node = graph
            .nodes
            .get(current.node_idx)
            .ok_or_else(|| RouteError::InvalidGraph("node missing".to_string()))?;
        let incoming = current.incoming.clone();
        let outgoing = outgoing_for_current.clone().unwrap_or_else(|| incoming.clone());
        hops_rev.push((
            node.clone(),
            L2Tag(incoming.0.clone()),
            L2Tag(outgoing.0.clone()),
        ));

        if let Some((prev_state, prev_outgoing)) = prev.get(&current) {
            current = prev_state.clone();
            outgoing_for_current = Some(prev_outgoing.clone());
        } else {
            break;
        }
    }

    hops_rev.reverse();
    Ok(hops_rev)
}

pub fn compute_hop_expiries(final_expiry: u64, deltas: &[u64]) -> Vec<u64> {
    let mut expiries = Vec::with_capacity(deltas.len() + 1);
    let mut current = final_expiry;
    expiries.push(current);
    for delta in deltas.iter().rev() {
        current = current.saturating_add(*delta);
        expiries.push(current);
    }
    expiries.reverse();
    expiries
}

fn node_fee_msat(node: &NodeAnnouncement, amount_msat: u64) -> u64 {
    let fee_ppm = (node.fee_ppm as u128)
        .saturating_mul(amount_msat as u128)
        / 1_000_000u128;
    let fee = node.fee_base_msat as u128 + fee_ppm;
    fee.min(u64::MAX as u128) as u64
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct StateKey {
    node_idx: usize,
    incoming: NetworkId,
}

#[derive(Clone, Debug)]
struct State {
    cost: u64,
    key: StateKey,
}

impl PartialEq for State {
    fn eq(&self, other: &Self) -> bool {
        self.cost == other.cost
    }
}

impl Eq for State {}

impl Ord for State {
    fn cmp(&self, other: &Self) -> Ordering {
        other.cost.cmp(&self.cost)
    }
}

impl PartialOrd for State {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
