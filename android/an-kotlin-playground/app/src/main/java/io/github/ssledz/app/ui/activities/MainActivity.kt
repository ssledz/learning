package io.github.ssledz.app.ui.activities

import android.os.Bundle
import android.util.Log
import android.view.View
import android.widget.Toast
import androidx.appcompat.app.AppCompatActivity
import io.github.ssledz.app.R
import kotlinx.android.synthetic.main.activity_main.*

class MainActivity : AppCompatActivity() {

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.activity_main)
        message.text = "Hello Kotlin!!!"

        Log.i("info", "Done creating the app")

    }

    fun onClick(v : View?) {

        Toast.makeText(this, "Button clicked", Toast.LENGTH_SHORT).show()
        Log.i("info", "User clicked")

    }
}
