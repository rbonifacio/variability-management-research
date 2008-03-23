package com.google.android.game2d.wolf;

import android.app.Activity;
import android.os.Bundle;

public class WolfActivator extends Activity {
	/** Called when the activity is first created. */
	@Override
	public void onCreate(Bundle icicle) {
		super.onCreate(icicle);
		ImageEngine.initialize(getResources());
		WolfFactory.initialize(this);
		WolfFactory.instance().getEngine().initialize();
		WolfFactory.instance().getEngine().loop();
	}
}