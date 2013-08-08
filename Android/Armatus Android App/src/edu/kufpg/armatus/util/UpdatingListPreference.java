package edu.kufpg.armatus.util;

import android.content.Context;
import android.preference.ListPreference;
import android.util.AttributeSet;

/**
 * Workaround that makes a {@link ListPreference} automatically update its summary
 * when its selected entry changes.
 */
public class UpdatingListPreference extends ListPreference {

	public UpdatingListPreference(Context context) {
		super(context);
	}

	public UpdatingListPreference(Context context, AttributeSet attrs) {
		super(context, attrs);
	}

	@Override
	public void setValue(final String value) {
		super.setValue(value);
		notifyChanged();
	}

}
