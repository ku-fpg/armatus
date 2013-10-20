package edu.kufpg.armatus;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import android.text.style.ClickableSpan;
import android.view.View;

import com.google.common.collect.Lists;

public class LineTestSpan extends ClickableSpan {

	private final int mStartIndex, mEndIndex;
	private LineTestSpan mParentSpan;
	private List<LineTestSpan> mChildSpans;

	public LineTestSpan(int startIndex, int endIndex, LineTestSpan parentSpan, LineTestSpan... childSpans) {
		super();
		mStartIndex = startIndex;
		mEndIndex = endIndex;
		mParentSpan = parentSpan;
		mChildSpans = Lists.newArrayList(childSpans);
	}

	public LineTestSpan(int startIndex, int endIndex, LineTestSpan parentSpan, Collection<? extends LineTestSpan> spans) {
		super();
		mStartIndex = startIndex;
		mEndIndex = endIndex;
		mParentSpan = parentSpan;
		mChildSpans = new ArrayList<LineTestSpan>(spans);
	}

	public void addChildSpan(LineTestSpan newChild) {
		mChildSpans.add(newChild);
	}

	public LineTestSpan getChildSpan(int index) {
		return mChildSpans.get(index);
	}

	public int getChildSpanCount() {
		return mChildSpans.size();
	}

	public int getEndIndex() {
		return mEndIndex;
	}

	public LineTestSpan getParentSpan() {
		return mParentSpan;
	}

	public int getStartIndex() {
		return mStartIndex;
	}

	public boolean hasChildSpans() {
		return mChildSpans != null && !mChildSpans.isEmpty();
	}

	public boolean hasParentSpan() {
		return mParentSpan != null;
	}

	public void setChildSpans(Collection<? extends LineTestSpan> newChildren) {
		mChildSpans = new ArrayList<LineTestSpan>(newChildren);
	}

	public void setParentSpan(LineTestSpan newParent) {
		mParentSpan = newParent;
	}

	@Override
	public boolean equals(Object o) {
		if (o instanceof LineTestSpan) {
			LineTestSpan other = (LineTestSpan) o;
			return mStartIndex == other.getStartIndex() &&
					mEndIndex == other.getEndIndex();
		} else {
			return false;
		}
	}

	@Override
	public void onClick(View widget) {}

}
