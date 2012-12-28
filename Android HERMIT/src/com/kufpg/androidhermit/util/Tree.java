package com.kufpg.androidhermit.util;

/*
 * Copyright 2010 Vivin Suresh Paliath
 * Distributed under the BSD License
 */

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

public class Tree<T> {

	private TreeNode<T> root;

	public Tree() {
		super();
	}
	
	public Tree(T data) {
		super();
		setRoot(new TreeNode<T>(data));
	}

	public enum TreeTraversalOrder {
		PRE_ORDER,
		POST_ORDER
	}

	public TreeNode<T> getRoot() {
		return this.root;
	}

	public void setRoot(TreeNode<T> root) {
		this.root = root;
	}

	public int getNumberOfNodes() {
		int numberOfNodes = 0;

		if(root != null) {
			numberOfNodes = auxiliaryGetNumberOfNodes(root) + 1; //1 for the root!
		}

		return numberOfNodes;
	}

	private int auxiliaryGetNumberOfNodes(TreeNode<T> node) {
		int numberOfNodes = node.getNumberOfChildren();

		for(TreeNode<T> child : node.getChildren()) {
			numberOfNodes += auxiliaryGetNumberOfNodes(child);
		}

		return numberOfNodes;
	}

	public boolean exists(T dataToFind) {
		return (find(dataToFind) != null);
	}

	public TreeNode<T> find(T dataToFind) {
		TreeNode<T> returnNode = null;

		if(root != null) {
			returnNode = auxiliaryFind(root, dataToFind);
		}

		return returnNode;
	}

	private TreeNode<T> auxiliaryFind(TreeNode<T> currentNode, T dataToFind) {
		TreeNode<T> returnNode = null;
		int i = 0;

		if (currentNode.getData().equals(dataToFind)) {
			returnNode = currentNode;
		}

		else if(currentNode.hasChildren()) {
			i = 0;
			while(returnNode == null && i < currentNode.getNumberOfChildren()) {
				returnNode = auxiliaryFind(currentNode.getChildAt(i), dataToFind);
				i++;
			}
		}

		return returnNode;
	}

	public boolean isEmpty() {
		return (root == null);
	}

	public List<TreeNode<T>> build(TreeTraversalOrder traversalOrder) {
		List<TreeNode<T>> returnList = null;

		if(root != null) {
			returnList = build(root, traversalOrder);
		}

		return returnList;
	}

	public List<TreeNode<T>> build(TreeNode<T> node, TreeTraversalOrder traversalOrder) {
		List<TreeNode<T>> traversalResult = new ArrayList<TreeNode<T>>();

		if(traversalOrder == TreeTraversalOrder.PRE_ORDER) {
			buildPreOrder(node, traversalResult);
		}

		else if(traversalOrder == TreeTraversalOrder.POST_ORDER) {
			buildPostOrder(node, traversalResult);
		}

		return traversalResult;
	}

	private void buildPreOrder(TreeNode<T> node, List<TreeNode<T>> traversalResult) {
		traversalResult.add(node);

		for(TreeNode<T> child : node.getChildren()) {
			buildPreOrder(child, traversalResult);
		}
	}

	private void buildPostOrder(TreeNode<T> node, List<TreeNode<T>> traversalResult) {
		for(TreeNode<T> child : node.getChildren()) {
			buildPostOrder(child, traversalResult);
		}

		traversalResult.add(node);
	}

	public Map<TreeNode<T>, Integer> buildWithDepth(TreeTraversalOrder traversalOrder) {
		Map<TreeNode<T>, Integer> returnMap = null;

		if(root != null) {
			returnMap = buildWithDepth(root, traversalOrder);
		}

		return returnMap;
	}

	public Map<TreeNode<T>, Integer> buildWithDepth(TreeNode<T> node, TreeTraversalOrder traversalOrder) {
		Map<TreeNode<T>, Integer> traversalResult = new LinkedHashMap<TreeNode<T>, Integer>();

		if(traversalOrder == TreeTraversalOrder.PRE_ORDER) {
			buildPreOrderWithDepth(node, traversalResult, 0);
		}

		else if(traversalOrder == TreeTraversalOrder.POST_ORDER) {
			buildPostOrderWithDepth(node, traversalResult, 0);
		}

		return traversalResult;
	}

	private void buildPreOrderWithDepth(TreeNode<T> node, Map<TreeNode<T>, Integer> traversalResult, int depth) {
		traversalResult.put(node, depth);

		for(TreeNode<T> child : node.getChildren()) {
			buildPreOrderWithDepth(child, traversalResult, depth + 1);
		}
	}

	private void buildPostOrderWithDepth(TreeNode<T> node, Map<TreeNode<T>, Integer> traversalResult, int depth) {
		for(TreeNode<T> child : node.getChildren()) {
			buildPostOrderWithDepth(child, traversalResult, depth + 1);
		}

		traversalResult.put(node, depth);
	}

	public String toString(TreeTraversalOrder traversalOrder) {
		String stringRepresentation = "";

		if(root != null) {
			stringRepresentation = build(traversalOrder).toString();
		}

		return stringRepresentation;
	}

	public String toStringWithDepth(TreeTraversalOrder traversalOrder) {

		String stringRepresentation = "";

		if(root != null) {
			stringRepresentation = buildWithDepth(traversalOrder).toString();
		}

		return stringRepresentation;
	}
}