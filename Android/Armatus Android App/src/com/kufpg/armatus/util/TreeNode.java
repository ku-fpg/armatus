package com.kufpg.armatus.util;

/*
 * Copyright 2010 Visin Suresh Paliath
 * Distributed under the BSD license
 */

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class TreeNode<T> {

	private T data;
	private List<TreeNode<T>> children;
	private TreeNode<T> parent;

	public TreeNode() {
		super();
		children = new ArrayList<TreeNode<T>>();
	}

	public TreeNode(T data) {
		this();
		setData(data);
	}

	public TreeNode<T> getParent() {
		return this.parent;
	}

	public List<TreeNode<T>> getChildren() {
		return this.children;
	}

	public int getNumberOfChildren() {
		return getChildren().size();
	}

	public boolean hasChildren() {
		return (getNumberOfChildren() > 0);
	}

	public void setChildren(List<TreeNode<T>> children) {
		for (TreeNode<T> child : children) {
			child.parent = this;
		}

		this.children = children;
	}

	public void addChild(TreeNode<T> child) {
		child.parent = this;
		children.add(child);
	}

	public void addChild(T data) {
		TreeNode<T> child = new TreeNode<T>(data);
		child.parent = this;
		children.add(child);
	}

	public void addChildren(TreeNode<T>... children) {
		for (TreeNode<T> child : children) {
			addChild(child);
		}
	}

	public void addChildren(T... data) {
		for (T datum : data) {
			addChild(datum);
		}
	}

	public void addChildAt(int index, TreeNode<T> child)
			throws IndexOutOfBoundsException {
		child.parent = this;
		children.add(index, child);
	}

	public void addChildAt(int index, T data) throws IndexOutOfBoundsException {
		TreeNode<T> child = new TreeNode<T>(data);
		child.parent = this;
		children.add(index, child);
	}

	public void addChildrenAt(int index, TreeNode<T>... children)
			throws IndexOutOfBoundsException {
		for (TreeNode<T> child : children) {
			addChildAt(index, child);
		}
	}

	public void addChildrenAt(int index, T... data) {
		for (T datum : data) {
			addChildAt(index, datum);
		}
	}

	public void removeChildren() {
		this.children = new ArrayList<TreeNode<T>>();
	}

	public void removeChildAt(int index) throws IndexOutOfBoundsException {
		children.remove(index);
	}

	public TreeNode<T> getChildAt(int index) throws IndexOutOfBoundsException {
		return children.get(index);
	}

	public T getData() {
		return this.data;
	}

	public void setData(T data) {
		this.data = data;
	}

	@Override
	public String toString() {
		return getData().toString();
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (obj == null) {
			return false;
		}
		if (getClass() != obj.getClass()) {
			return false;
		}
		TreeNode<?> other = (TreeNode<?>) obj;
		if (data == null) {
			if (other.data != null) {
				return false;
			}
		} else if (!data.equals(other.data)) {
			return false;
		}
		return true;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((data == null) ? 0 : data.hashCode());
		return result;
	}

	public String toStringVerbose() {
		String stringRepresentation = getData().toString() + ":[";

		for (TreeNode<T> node : getChildren()) {
			stringRepresentation += node.getData().toString() + ", ";
		}

		Pattern pattern = Pattern.compile(", $", Pattern.DOTALL);
		Matcher matcher = pattern.matcher(stringRepresentation);

		stringRepresentation = matcher.replaceFirst("");
		stringRepresentation += "]";

		return stringRepresentation;
	}
}
